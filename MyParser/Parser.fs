namespace MyParser

open Microsoft.FSharp.Collections

type identifier = string
type index = int
type Hashtable<'k, 'v> = System.Collections.Generic.Dictionary<'k, 'v>

type arithmetic =
    | MpAdd
    | MpSubtract
    | MpMultiply
    | MpDivide
    | MpRest

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
    | MpDouble of double
    | MpString of string
    | MpChar of char
    | MpArrayValue of value[]
    | MpFuncValue of identifier

type expr =
    | MpIndex of identifier * expr list
    | MpArrayL of expr[]
    | MpArrayD of expr * expr
    | MpSlice of identifier * expr * expr
    | MpLiteral of value
    | MpVar of identifier
    | MpNeg of expr
    | MpArithmetic of expr * arithmetic * expr
    | MpComparison of expr * comparison * expr
    | MpLogical of expr * logical * expr
    | MpInvoke of identifier * expr list
    | MpReservedFunc0 of identifier
    | MpReservedFunc1 of identifier * expr

type assign =
    | Set of identifier * expr
    | SetE of expr * expr

type instruction =
    | MpFunc of identifier * identifier list
    | MpAssign of assign
    | MpExpr of expr
    | MpFor of identifier * expr * expr * expr
    | MpIf of expr
    | MpElIf of expr
    | MpElse
    | MpWhile of expr
    | MpEnd
    | MpReturn of expr
    | MpBreak

open System
open FParsec

module Parser =

    let reservedWords =
        [ "for"
          "while"
          "if"
          "else"
          "elif"
          "func"
          "true"
          "false"
          "return"
          "func"
          "break" ]

    let reservedFunctions0 = [ "input" ]
    let reservedFunctions1 = [ "printLn"; "printL"; "int"; "double"; "str" ]

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

    let quotedChar =
        let quote = pchar '\'' <?> "quote"
        let jChar = mpUnEscapedChar <|> mpEscapedChar <|> mpUnicodeChar

        quote >>. jChar .>> quote

    let mpChar: Parser<expr, unit> =
        quotedChar <?> "quoted char" |>> MpChar |>> MpLiteral


    let mpNum: Parser<expr, unit> =
        let numberFormat = NumberLiteralOptions.AllowFraction

        numberLiteral numberFormat "number"
        |>> fun nl ->
                if nl.IsInteger then
                    MpLiteral(MpInt(int nl.String))
                else
                    MpLiteral(MpDouble(double nl.String))

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

        let reservedWord =
            choice (reservedWords @ reservedFunctions0 @ reservedFunctions1 |> List.map pstring)

        notFollowedBy reservedWord
        .>>. many1Satisfy2L isIdentifierFirstChar isIdentifierChar "identifier"
        |>> snd


    let mpIdentifier_ws = mpIdentifier .>> ws

    let mpVar: Parser<expr, unit> = mpIdentifier |>> MpVar

    let mpArray, mpArrayR = createParserForwardedToRef ()

    let mpIndex, mpIndexR = createParserForwardedToRef ()

    let mpInvoke, mpInvokeR = createParserForwardedToRef ()

    let mpReservedFunc, mpReservedFuncR = createParserForwardedToRef ()

    let mpSlice, mpSliceR = createParserForwardedToRef ()

    let mpValue =
        choice
            [ attempt mpSlice
              attempt mpIndex
              attempt mpInvoke
              attempt mpReservedFunc
              mpNum
              mpString
              mpNull
              mpVar
              mpBool
              mpChar ]

    type Assoc = Associativity

    let oppA = OperatorPrecedenceParser<expr, unit, unit>()

    let rec mpArithmetic = oppA.ExpressionParser

    let termA = (mpValue .>> ws) <|> between (str_ws "(") (str_ws ")") mpArithmetic

    oppA.TermParser <- termA
    oppA.AddOperator(InfixOperator("+", ws, 1, Assoc.Left, (fun x y -> MpArithmetic(x, MpAdd, y))))
    oppA.AddOperator(InfixOperator("-", ws, 1, Assoc.Left, (fun x y -> MpArithmetic(x, MpSubtract, y))))
    oppA.AddOperator(InfixOperator("*", ws, 2, Assoc.Left, (fun x y -> MpArithmetic(x, MpMultiply, y))))
    oppA.AddOperator(InfixOperator("/", ws, 2, Assoc.Left, (fun x y -> MpArithmetic(x, MpDivide, y))))
    oppA.AddOperator(PrefixOperator("-", ws, 2, true, MpNeg))
    oppA.AddOperator(InfixOperator("%", ws, 2, Assoc.Left, (fun x y -> MpArithmetic(x, MpRest, y))))

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

    let termL = (mpComparison .>> ws) <|> between (str_ws "(") (str_ws ")") mpLogical

    oppL.TermParser <- termL
    oppL.AddOperator(InfixOperator("&&", ws, 1, Assoc.Left, (fun x y -> MpLogical(x, MpAnd, y))))
    oppL.AddOperator(InfixOperator("||", ws, 1, Assoc.Left, (fun x y -> MpLogical(x, MpOr, y))))
    oppL.AddOperator(InfixOperator("^^", ws, 1, Assoc.Left, (fun x y -> MpLogical(x, MpXor, y))))

    mpIndexR.Value <- pipe2 mpIdentifier (many1 (str_ws "[" >>. mpArithmetic .>> str_ws "]")) (fun x y -> MpIndex(x, y))

    let mpSliceInd =
        (pipe3 mpArithmetic (str_ws ":") mpArithmetic (fun x _ y -> (x, y)))

    mpSliceR.Value <- pipe4 mpIdentifier (str_ws "[") mpSliceInd (pstring "]") (fun s _ (x, y) _ -> MpSlice(s, x, y))

    let mpExpr = mpLogical <|> mpComparison <|> mpArithmetic <|> mpArray

    let mpArrayL =
        (between (pchar '[') (pchar ']') (sepBy (ws >>. mpExpr .>> ws) (pchar ',')))
        |>> List.toArray
        |>> MpArrayL

    let mpArrayD =
        pipe5 (str_ws "[") (mpExpr .>> ws) (str_ws ";") (mpExpr .>> ws) (pstring "]") (fun _ x _ y _ -> MpArrayD(x, y))

    mpArrayR.Value <- attempt mpArrayL <|> attempt mpArrayD


    let mpInvokeVar =
        between (str_ws "(") (pstring ")") (sepBy (ws >>. mpExpr .>> ws) (pchar ','))

    mpInvokeR.Value <- pipe2 mpIdentifier mpInvokeVar (fun x y -> MpInvoke(x, y))

    let mpReservedFuncIdentifier1: Parser<string, unit> =
        choice (reservedFunctions1 |> List.map pstring)

    let mpReservedFunc1 =
        pipe2 mpReservedFuncIdentifier1 (between (str_ws "(") (pstring ")") (ws >>. mpExpr .>> ws)) (fun x y ->
            MpReservedFunc1(x, y))

    let mpReservedFuncIdentifier0: Parser<string, unit> =
        choice (reservedFunctions0 |> List.map pstring)

    let mpReservedFunc0 =
        pipe2 mpReservedFuncIdentifier0 (between (str_ws "(") (pstring ")") ws) (fun x _ -> MpReservedFunc0 x)

    mpReservedFuncR.Value <- mpReservedFunc0 <|> mpReservedFunc1

    let mpAssign =
        pipe3 mpIdentifier (ws >>. (str_ws "=")) mpExpr (fun id _ e -> MpAssign(Set(id, e)))

    let mpAssignE =
        pipe3 (mpIndex .>> ws) (ws >>. (str_ws "=")) mpExpr (fun id _ e -> MpAssign(SetE(id, e)))

    let mpExprInstr = mpExpr |>> MpExpr

    let mpRange3 =
        pipe5 mpArithmetic (str_ws ",") mpArithmetic (str_ws ",") mpArithmetic (fun x _ y _ z -> (x, y, z))

    let mpRange2 =
        pipe3 mpArithmetic (str_ws ",") mpArithmetic (fun x _ y -> (x, y, MpLiteral(MpInt 1)))

    let mpRange1 =
        mpArithmetic |>> (fun x -> (MpLiteral(MpInt 0), x, MpLiteral(MpInt 1)))

    let mpRange = attempt mpRange3 <|> attempt mpRange2 <|> attempt mpRange1

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

    let mpFuncVar =
        between (str_ws "(") (pstring ")") (sepBy (ws >>. mpIdentifier .>> ws) (pchar ','))

    let mpFunc =
        pipe4 (str_ws "func") mpIdentifier mpFuncVar (wsl >>. pstring "{") (fun _ x y _ -> MpFunc(x, y))

    let mpReturnValue = pipe2 (str_ws "return") mpExpr (fun _ -> MpReturn)

    let mpReturnVoid = pstring "return" |>> (fun _ -> MpReturn(MpLiteral MpNull))

    let mpReturn = attempt mpReturnValue <|> mpReturnVoid

    let mpBreak: Parser<instruction, unit> = pstring "break" |>> (fun _ -> MpBreak)

    let mpInstruct =
        [ mpAssign; mpAssignE; mpExprInstr; mpReturn; mpBreak ]
        |> List.map attempt
        |> choice

    let mpBlockInstruct =
        [ mpWhile; mpFor; mpEnd; mpIf; mpElIf; mpElse; mpFunc ]
        |> List.map attempt
        |> choice

    type Line =
        | Blank
        | Instruction of instruction

    let mpComment = pchar '#' >>. skipManySatisfy (fun c -> c <> '\n') >>. pchar '\n'

    let mpEndInst: Parser<char, unit> = wsl >>. pchar ';'

    let mpEol = mpComment <|> (pchar '\n')

    let mpInstruction =
        ws >>. ((mpInstruct .>> mpEndInst) <|> mpBlockInstruct) |>> Instruction

    let mpBlank = ws >>. mpEol |>> (fun _ -> Blank)
    let mpLines = many (attempt mpInstruction <|> mpBlank) .>> eof

    let mpParse (program: string) =
        match run mpLines program with
        | Success (result, _, _) ->
            result
            |> List.choose (function
                | Instruction i -> Some i
                | Blank -> None)
            |> List.toArray
        | Failure (errorMsg, _, _) -> failwith errorMsg
