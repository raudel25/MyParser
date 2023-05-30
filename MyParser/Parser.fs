namespace MyParser

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
    | MpArray of Hashtable<value, value>

type expr =
    | MpLiteral of value
    | MpVar of identifier
    | MpGetAt of location
    | MpFunc of invoke
    | MpNeg of expr
    | MpArithmetic of expr * arithmetic * expr
    | MpComparison of expr * comparison * expr
    | MpLogical of expr * logical * expr

and location = Location of identifier * expr list

and invoke =
    | MpMethod of string * string * expr[]
    | MpPropertyGet of string * string

type assign = Set of identifier * expr

type instruction =
    | MpPrint of expr
    | MpAssign of assign
    | MpSetAt of location * expr
    | MpPropertySet of string * string * expr
    | MpAction of invoke
    | MpFor of assign * expr * expr
    | MpEndFor
    | MpIf of expr
    | MpElseIf of expr
    | MpElse
    | MpEndIf
    | MpWhile of expr
    | MpEnd
    | MpSub of identifier
    | MpEndSub
    | MpGoSub of identifier
    | MpLabel of label
    | MpGoto of label

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
            [ "for"; "while"; "if"; "else"; "elif"; "func"; "print"; "true"; "false" ]

        let reservedWord = choice (reservedWords |> List.map pstring)

        notFollowedBy reservedWord
        .>>. many1Satisfy2L isIdentifierFirstChar isIdentifierChar "identifier"
        |>> snd


    let mpIdentifier_ws = mpIdentifier .>> ws
    let mpVar = mpIdentifier |>> MpVar

    let mpValue = mpNum <|> mpString <|> mpNull <|> mpVar


    type Assoc = Associativity

    let oppA = OperatorPrecedenceParser<expr, unit, unit>()
    let mpArithmetic = oppA.ExpressionParser

    let termA = (mpValue .>> ws) <|> between (str_ws "(") (str_ws ")") mpArithmetic

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
        pipe3 (str_ws "print(") (mpComparison <|>mpLogical<|> mpArithmetic) (pstring ")") (fun _ e _ ->
            MpPrint e)

    let mpAssign =
        pipe3 mpIdentifier_ws (str_ws "=") (mpComparison <|>mpLogical<|> mpArithmetic) (fun id _ e ->
            MpAssign(Set(id, e)))

    let mpWhile =
        pipe5 (str_ws "while") (str_ws "(") mpLogical (str_wsl ")") (pstring "{") (fun _ _ e _ _ -> MpWhile e)
        
    let mpEnd: Parser<instruction, unit> = pstring "}" |>> (fun _ -> MpEnd)


    let mpInstruct = [ mpAssign; mpPrint ] |> List.map attempt |> choice
    let mpBlockInstruct=[ mpWhile;mpEnd] |> List.map attempt |> choice
   
    type Line =
        | Blank
        | Instruction of instruction

    let mpComment = pchar '#' >>. skipManySatisfy (fun c -> c <> '\n') >>. pchar '\n'

    let mpEndInst: Parser<char, unit> = wsl >>. pchar ';'
    
    let mpEol = mpComment <|> (pchar '\n')
    let mpInstruction = ws >>. ((mpInstruct .>> mpEndInst) <|> mpBlockInstruct) |>> Instruction
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
