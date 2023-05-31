namespace MyParser

open Microsoft.FSharp.Collections

type label = string
type identifier = string
type index = int
type Hashtable<'k, 'v> = System.Collections.Generic.Dictionary<'k, 'v>

type arithmetic =
    | MpAdd
    | MpSubtract
    | MpMultiply
    | MpDivide

type comparison =
    | MpEq
    | MpNe
    | MpLt
    | MpGt
    | MpLe
    | MpGe

type logical =
    | MpAnd
    | MpOr
    | MpXor

type value =
    | MpNull
    | MpBool of bool
    | MpInt of int
    | MpFloat of float
    | MpString of string
    | MpArrayValue of value[]

type expr =
    | MpIndex of identifier * expr list
    | MpArray of expr[]
    | MpLiteral of value
    | MpVar of identifier
    | MpNeg of expr
    | MpArithmetic of expr * arithmetic * expr
    | MpComparison of expr * comparison * expr
    | MpLogical of expr * logical * expr

and location = Location of identifier * expr list

and invoke =
    | MpMethod of string * string * expr[]
    | MpPropertyGet of string * string

type assign =
    | Set of identifier * expr
    | SetE of expr * expr

type instruction =
    | MpPrint of expr
    | MpAssign of assign
    | MpExpr of expr
    | MpFor of identifier * int * int * int
    | MpIf of expr
    | MpElIf of expr
    | MpElse
    | MpWhile of expr
    | MpEnd

open System
open FParsec

module Parser =

    let (>>%) p x = p |>> (fun _ -> x)

    let mpNull: Parser<expr, unit> = pstring "null" >>% MpNull <?> "null" |>> MpLiteral

    let mpBool: Parser<expr, unit> =
        let mpTrue = pstring "true" >>% MpBool true
        let mpFalse = pstring "false" >>% MpBool false

        mpTrue <|> mpFalse <?> "bool" |>> MpLiteral

    let mpUnEscapedChar: Parser<char, unit> =
        satisfy (fun ch -> ch <> '\\' && ch <> '\"')

    let mpEscapedChar =
        [ ("\\\"", '\"')
          ("\\\\", '\\')
          ("\\/", '/')
          ("\\b", '\b')
          ("\\f", '\f')
          ("\\n", '\n')
          ("\\r", '\r')
          ("\\t", '\t') ]

        |> List.map (fun (toMatch, result) -> pstring toMatch >>% result)
        |> choice
        <?> "escaped char"

    let mpUnicodeChar =
        let backslash = pchar '\\'
        let uChar = pchar 'u'
        let hexDigit = anyOf ([ '0' .. '9' ] @ [ 'A' .. 'F' ] @ [ 'a' .. 'f' ])
        let fourHexDigits = hexDigit .>>. hexDigit .>>. hexDigit .>>. hexDigit

        let convertToChar (((h1, h2), h3), h4) =
            let str = $"%c{h1}%c{h2}%c{h3}%c{h4}"
            Int32.Parse(str, Globalization.NumberStyles.HexNumber) |> char

        backslash >>. uChar >>. fourHexDigits |>> convertToChar

    let quotedString =
        let quote = pchar '\"' <?> "quote"
        let jChar = mpUnEscapedChar <|> mpEscapedChar <|> mpUnicodeChar

        quote >>. manyChars jChar .>> quote

    let mpString: Parser<expr, unit> =
        quotedString <?> "quoted string" |>> MpString |>> MpLiteral

    let mpNum: Parser<expr, unit> =
        let numberFormat = NumberLiteralOptions.AllowFraction

        numberLiteral numberFormat "number"
        |>> fun nl ->
                if nl.IsInteger then
                    MpLiteral(MpInt(int nl.String))
                else
                    MpLiteral(MpFloat(float nl.String))

    let ws: Parser<unit, unit> =
        skipManySatisfy (fun c -> c = ' ' || c = '\t' || c = '\r')

    let wsl: Parser<unit, unit> =
        skipManySatisfy (fun c -> c = '\n' || c = ' ' || c = '\t' || c = '\r')

    let str_ws s = pstring s .>> ws
    let str_ws1 s = pstring s .>> spaces1
    let str_wsl s = pstring s .>> wsl
    let str_wsl1 s = pstring s .>> spaces1

    let mpIdentifier: Parser<string, unit> =
        let isIdentifierFirstChar c = isLetter c || c = '_'
        let isIdentifierChar c = isLetter c || isDigit c || c = '_'

        let reservedWords =
            [ "for"
              "while"
              "if"
              "else"
              "elif"
              "func"
              "print"
              "true"
              "false"
              "array" ]

        let reservedWord = choice (reservedWords |> List.map pstring)

        notFollowedBy reservedWord
        .>>. many1Satisfy2L isIdentifierFirstChar isIdentifierChar "identifier"
        |>> snd


    let mpIdentifier_ws = mpIdentifier .>> ws

    let mpVar: Parser<expr, unit> =
        (notFollowedBy (mpIdentifier .>>. pstring "[") .>>. mpIdentifier)
        |>> (fun (_, y) -> (MpVar y))

    let mpValue = mpNum <|> mpString <|> mpNull <|> mpVar <|> mpBool

    let mpValueA = mpNum <|> mpVar

    let mpArrayL =
        (between (pchar '[') (pchar ']') (sepBy (ws >>. mpValue .>> ws) (pchar ',')))
        |>> List.toArray

    let mpArrayV =
        pipe5 (str_ws "array(") (mpValue .>> ws) (str_ws ",") (puint32 .>> ws) (pstring ")") (fun _ x _ y _ ->
            Array.create (int y) x)

    let mpArray = mpArrayL <|> mpArrayV |>> MpArray

    type Assoc = Associativity

    let oppA = OperatorPrecedenceParser<expr, unit, unit>()

    let rec mpArithmetic = oppA.ExpressionParser

    and mpGetIndex =
        pipe2 mpIdentifier (many1 (str_ws "[" >>. mpArithmetic .>> str_ws "]")) (fun x y -> MpIndex(x, y))

    let termA =
        ((mpValueA <|> mpGetIndex) .>> ws)
        <|> between (str_ws "(") (str_ws ")") mpArithmetic

    oppA.TermParser <- termA
    oppA.AddOperator(InfixOperator("+", ws, 1, Assoc.Left, (fun x y -> MpArithmetic(x, MpAdd, y))))
    oppA.AddOperator(InfixOperator("-", ws, 1, Assoc.Left, (fun x y -> MpArithmetic(x, MpSubtract, y))))
    oppA.AddOperator(InfixOperator("*", ws, 2, Assoc.Left, (fun x y -> MpArithmetic(x, MpMultiply, y))))
    oppA.AddOperator(InfixOperator("/", ws, 2, Assoc.Left, (fun x y -> MpArithmetic(x, MpDivide, y))))
    oppA.AddOperator(PrefixOperator("-", ws, 2, true, MpNeg))

    let oppC = OperatorPrecedenceParser<expr, unit, unit>()
    let mpComparison = oppC.ExpressionParser
    let termC = (mpArithmetic .>> ws) <|> between (str_ws "(") (str_ws ")") mpComparison
    oppC.TermParser <- termC
    oppC.AddOperator(InfixOperator("==", ws, 1, Assoc.Left, (fun x y -> MpComparison(x, MpEq, y))))
    oppC.AddOperator(InfixOperator("!=", ws, 1, Assoc.Left, (fun x y -> MpComparison(x, MpNe, y))))
    oppC.AddOperator(InfixOperator("<=", ws, 2, Assoc.Left, (fun x y -> MpComparison(x, MpLe, y))))
    oppC.AddOperator(InfixOperator(">=", ws, 2, Assoc.Left, (fun x y -> MpComparison(x, MpGe, y))))
    oppC.AddOperator(InfixOperator("<", ws, 2, Assoc.Left, (fun x y -> MpComparison(x, MpLt, y))))
    oppC.AddOperator(InfixOperator(">", ws, 2, Assoc.Left, (fun x y -> MpComparison(x, MpGt, y))))

    let oppL = OperatorPrecedenceParser<expr, unit, unit>()
    let mpLogical = oppL.ExpressionParser

    let termL =
        ((mpComparison <|> mpBool) .>> ws)
        <|> between (str_ws "(") (str_ws ")") mpLogical

    oppL.TermParser <- termL
    oppL.AddOperator(InfixOperator("&&", ws, 1, Assoc.Left, (fun x y -> MpLogical(x, MpAnd, y))))
    oppL.AddOperator(InfixOperator("||", ws, 1, Assoc.Left, (fun x y -> MpLogical(x, MpOr, y))))
    oppL.AddOperator(InfixOperator("^^", ws, 1, Assoc.Left, (fun x y -> MpLogical(x, MpXor, y))))

    let mpPrint =
        pipe3 (str_ws "print(") (mpLogical <|> mpComparison <|> mpArithmetic <|> mpArray) (pstring ")") (fun _ e _ ->
            MpPrint e)

    let mpAssign =
        pipe3 mpIdentifier (str_ws "=") (mpLogical <|> mpComparison <|> mpArithmetic <|> mpArray) (fun id _ e ->
            MpAssign(Set(id, e)))

    let mpAssignE =
        pipe3 (mpGetIndex .>> ws) (str_ws "=") (mpLogical <|> mpComparison <|> mpArithmetic <|> mpArray) (fun id _ e ->
            MpAssign(SetE(id, e)))

    let mpExpr = mpLogical <|> mpComparison <|> mpArithmetic <|> mpArray |>> MpExpr

    let mpRange3 =
        pipe5 pint32 (str_ws ",") pint32 (str_ws ",") pint32 (fun x _ y _ z -> (x, y, z))

    let mpRange2 =
        notFollowedBy mpRange3
        .>>. pipe3 pint32 (str_ws ",") pint32 (fun x _ y -> (x, y, 1))
        |>> snd

    let mpRange1 =
        (notFollowedBy mpRange2 .>>. (pint32 |>> (fun x -> (0, x, 1)))) |>> snd

    let mpRange = mpRange1 <|> mpRange2 <|> mpRange3

    let mpWhile =
        pipe5 (str_ws "while") (str_ws "(") mpLogical (str_wsl ")") (pstring "{") (fun _ _ e _ _ -> MpWhile e)

    let mpFor: Parser<instruction, unit> =
        pipe5 (str_ws "for") mpIdentifier_ws (str_ws "in") mpRange (wsl >>. pstring "{") (fun _ s _ (x, y, z) _ ->
            MpFor(s, x, y, z))

    let mpIf =
        pipe5 (str_ws "if") (str_ws "(") mpLogical (str_wsl ")") (pstring "{") (fun _ _ e _ _ -> MpIf e)

    let mpElIf =
        pipe5 (str_ws "elif") (str_ws "(") mpLogical (str_wsl ")") (pstring "{") (fun _ _ e _ _ -> MpElIf e)

    let mpElse = pipe2 (str_wsl "else") (pstring "{") (fun _ _ -> MpElse)

    let mpEnd: Parser<instruction, unit> = pstring "}" |>> (fun _ -> MpEnd)

    let mpInstruct =
        [ mpAssign; mpAssignE; mpPrint; mpExpr ] |> List.map attempt |> choice

    let mpBlockInstruct =
        [ mpWhile; mpFor; mpEnd; mpIf; mpElIf; mpElse ] |> List.map attempt |> choice

    type Line =
        | Blank
        | Instruction of instruction

    let mpComment = pchar '#' >>. skipManySatisfy (fun c -> c <> '\n') >>. pchar '\n'

    let mpEndInst: Parser<char, unit> = wsl >>. pchar ';'

    let mpEol = mpComment <|> (pchar '\n')

    let mpInstruction =
        ws >>. ((mpInstruct .>> mpEndInst) <|> mpBlockInstruct) |>> Instruction

    let mpBlank = ws >>. mpEol |>> (fun _ -> Blank)
    let mpLines = many (mpInstruction <|> mpBlank) .>> eof

    let mpParse (program: string) =
        match run mpLines program with
        | Success (result, _, _) ->
            printfn "Ok"

            result
            |> List.choose (function
                | Instruction i -> Some i
                | Blank -> None)
            |> List.toArray
        | Failure (errorMsg, _, _) -> failwith errorMsg
