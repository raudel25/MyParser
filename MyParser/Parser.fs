namespace MyParser

open FParsec
open System.Collections.Generic

type identifier = string
type index = int
type Hashtable<'k, 'v> = Dictionary<'k, 'v>

type arithmetic =
    | MpAdd
    | MpSubtract
    | MpMultiply
    | MpDivide
    | MpRest
    | MpArithmeticAnd
    | MpArithmeticOr
    | MpArithmeticXor

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
    | MpTupleValue of value[]
    | MpArrayValue of value[]
    | MpFuncValue of identifier * identifier list * List<identifier> * int * int
    | MpStructValue of identifier * Hashtable<identifier, value>

type exprT =
    | MpIdentProp of identifier * property list
    | MpTuple of expr[]
    | MpArrayL of expr[]
    | MpArrayD of expr * expr
    | MpSlice of expr * expr * expr
    | MpLiteral of value
    | MpVar of identifier
    | MpNeg of expr
    | MpArithmetic of expr * arithmetic * expr
    | MpComparison of expr * comparison * expr
    | MpLogical of expr * logical * expr
    | MpInvoke of expr * expr list
    | MpReservedFunc0 of identifier
    | MpReservedFunc1 of identifier * expr
    | MpTernary of expr * expr * expr
    | MpStructConst of identifier * expr list

and expr = exprT * Position

and property =
    | MpIndexA of expr
    | MpProperty of identifier * Position
    | MpIndexT of int * Position

type assign = Set of expr * expr

type instruction =
    | MpStruct of identifier * identifier list * Position
    | MpFunc of identifier * (identifier * Position) list * Position
    | MpAssign of assign
    | MpExpr of expr
    | MpFor of identifier * expr * expr * expr * Position
    | MpIf of expr * Position
    | MpElIf of expr * Position
    | MpElse of Position
    | MpWhile of expr * Position
    | MpStart
    | MpEnd
    | MpReturn of expr
    | MpBreak of Position

open System

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

    let mpPosition mp =
        getPosition .>>. mp |>> (fun (x, y) -> (y, x))

    let mpNull: Parser<expr, unit> =
        pstring "null" >>% MpNull <?> "null" |>> MpLiteral |> mpPosition

    let mpBool: Parser<expr, unit> =
        let mpTrue = pstring "true" >>% MpBool true
        let mpFalse = pstring "false" >>% MpBool false

        mpTrue <|> mpFalse <?> "bool" |>> MpLiteral |> mpPosition

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
        quotedString <?> "quoted string" |>> MpString |>> MpLiteral |> mpPosition

    let quotedChar =
        let quote = pchar '\'' <?> "quote"
        let jChar = mpUnEscapedChar <|> mpEscapedChar <|> mpUnicodeChar

        quote >>. jChar .>> quote

    let mpChar: Parser<expr, unit> =
        quotedChar <?> "quoted char" |>> MpChar |>> MpLiteral |> mpPosition


    let mpNum: Parser<expr, unit> =
        let numberFormat = NumberLiteralOptions.AllowFraction

        numberLiteral numberFormat "number"
        |>> fun nl ->
                if nl.IsInteger then
                    MpLiteral(MpInt(int nl.String))
                else
                    MpLiteral(MpDouble(double nl.String))
        |> mpPosition

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

    let mpVar: Parser<expr, unit> = mpIdentifier <?> "variable" |>> MpVar |> mpPosition

    let mpArray, mpArrayR = createParserForwardedToRef ()

    let mpTuple, mpTupleR = createParserForwardedToRef ()

    let mpStructConst, mpStructConstR = createParserForwardedToRef ()

    let mpIdentProp, mpIdentPropR = createParserForwardedToRef ()

    let mpInvoke, mpInvokeR = createParserForwardedToRef ()

    let mpReservedFunc, mpReservedFuncR = createParserForwardedToRef ()

    let mpSlice, mpSliceR = createParserForwardedToRef ()

    let mpTernary, mpTernaryR = createParserForwardedToRef ()

    let mpValue =
        [ mpInvoke
          mpReservedFunc
          mpIdentProp
          mpNum
          mpString
          mpNull
          mpVar
          mpBool
          mpChar ]
        |> List.map attempt
        |> choice

    type Assoc = Associativity

    let oppA = OperatorPrecedenceParser<expr, unit, unit>()

    let rec mpArithmetic = oppA.ExpressionParser

    let termA = (mpValue .>> ws) <|> between (str_ws "(") (str_ws ")") mpArithmetic

    oppA.TermParser <- termA
    oppA.AddOperator(InfixOperator("+", ws, 1, Assoc.Left, (fun (x, p) y -> MpArithmetic((x, p), MpAdd, y), p)))
    oppA.AddOperator(InfixOperator("-", ws, 1, Assoc.Left, (fun (x, p) y -> MpArithmetic((x, p), MpSubtract, y), p)))
    oppA.AddOperator(InfixOperator("*", ws, 2, Assoc.Left, (fun (x, p) y -> MpArithmetic((x, p), MpMultiply, y), p)))
    oppA.AddOperator(InfixOperator("/", ws, 2, Assoc.Left, (fun (x, p) y -> MpArithmetic((x, p), MpDivide, y), p)))
    oppA.AddOperator(PrefixOperator("-", ws, 2, true, (fun (x, p) -> MpNeg(x, p), p)))
    oppA.AddOperator(InfixOperator("%", ws, 2, Assoc.Left, (fun (x, p) y -> MpArithmetic((x, p), MpRest, y), p)))

    oppA.AddOperator(
        InfixOperator("&", ws, 3, Assoc.Left, (fun (x, p) y -> MpArithmetic((x, p), MpArithmeticAnd, y), p))
    )

    oppA.AddOperator(
        InfixOperator("|", ws, 3, Assoc.Left, (fun (x, p) y -> MpArithmetic((x, p), MpArithmeticOr, y), p))
    )

    oppA.AddOperator(
        InfixOperator("^", ws, 3, Assoc.Left, (fun (x, p) y -> MpArithmetic((x, p), MpArithmeticXor, y), p))
    )

    let oppC = OperatorPrecedenceParser<expr, unit, unit>()
    let mpComparison = oppC.ExpressionParser

    let termC = (mpArithmetic .>> ws) <|> between (str_ws "(") (str_ws ")") mpComparison

    oppC.TermParser <- termC
    oppC.AddOperator(InfixOperator("==", ws, 1, Assoc.Left, (fun (x, p) y -> MpComparison((x, p), MpEq, y), p)))
    oppC.AddOperator(InfixOperator("!=", ws, 1, Assoc.Left, (fun (x, p) y -> MpComparison((x, p), MpNe, y), p)))
    oppC.AddOperator(InfixOperator("<=", ws, 2, Assoc.Left, (fun (x, p) y -> MpComparison((x, p), MpLe, y), p)))
    oppC.AddOperator(InfixOperator(">=", ws, 2, Assoc.Left, (fun (x, p) y -> MpComparison((x, p), MpGe, y), p)))
    oppC.AddOperator(InfixOperator("<", ws, 2, Assoc.Left, (fun (x, p) y -> MpComparison((x, p), MpLt, y), p)))
    oppC.AddOperator(InfixOperator(">", ws, 2, Assoc.Left, (fun (x, p) y -> MpComparison((x, p), MpGt, y), p)))

    let oppL = OperatorPrecedenceParser<expr, unit, unit>()
    let mpLogical = oppL.ExpressionParser

    let termL = (mpComparison .>> ws) <|> between (str_ws "(") (str_ws ")") mpLogical

    oppL.TermParser <- termL
    oppL.AddOperator(InfixOperator("&&", ws, 1, Assoc.Left, (fun (x, p) y -> MpLogical((x, p), MpAnd, y), p)))
    oppL.AddOperator(InfixOperator("||", ws, 1, Assoc.Left, (fun (x, p) y -> MpLogical((x, p), MpOr, y), p)))
    oppL.AddOperator(InfixOperator("^^", ws, 1, Assoc.Left, (fun (x, p) y -> MpLogical((x, p), MpXor, y), p)))

    let mpIndexA = between (str_ws "[") (ws >>. pstring "]") mpArithmetic |>> MpIndexA

    let mpIndexT =
        getPosition .>>. (((pchar '.') .>>. pint32) |>> snd)
        |>> (fun (x, y) -> y, x)
        |>> MpIndexT

    let mpProperty =
        getPosition .>>. ((pchar '.') .>>. mpIdentifier |>> snd)
        |>> (fun (x, y) -> y, x)
        |>> MpProperty

    mpIdentPropR.Value <-
        pipe2 mpIdentifier (many1 (attempt mpIndexA <|> attempt mpIndexT <|> attempt mpProperty)) (fun x y ->
            MpIdentProp(x, y))
        |> mpPosition

    let mpExpr =
        [ mpStructConst
          mpTernary
          mpSlice
          mpLogical
          mpComparison
          mpArithmetic
          mpArray
          mpTuple ]
        |> List.map attempt
        |> choice

    mpTernaryR.Value <-
        pipe5 mpLogical (str_ws "?") mpExpr (str_ws ":") mpExpr (fun (l, p) _ e1 _ e2 -> MpTernary((l, p), e1, e2), p)

    mpTupleR.Value <-
        (between (pchar '(') (pchar ')') (sepBy (wsl >>. mpExpr .>> wsl) (pchar ',')))
        |>> List.toArray
        |>> MpTuple
        |> mpPosition

    let mpArrayL =
        (between (pchar '[') (pchar ']') (sepBy (wsl >>. mpExpr .>> wsl) (pchar ',')))
        |>> List.toArray
        |>> MpArrayL
        |> mpPosition

    let mpArrayD =
        pipe5 (str_ws "[") (mpExpr .>> ws) (str_ws ";") (mpExpr .>> ws) (pstring "]") (fun _ x _ y _ -> MpArrayD(x, y))
        |> mpPosition

    mpArrayR.Value <- attempt mpArrayL <|> attempt mpArrayD


    let mpSetGet = (attempt mpIdentProp <|> mpVar)

    let mpSliceInd =
        (pipe3 mpArithmetic (str_ws ":") mpArithmetic (fun x _ y -> (x, y)))

    mpSliceR.Value <-
        pipe4 mpSetGet (str_ws "[") mpSliceInd (pstring "]") (fun s _ (x, y) _ -> MpSlice(s, x, y))
        |> mpPosition

    let mpStructExpr =
        between (str_wsl "{") (pstring "}") (sepBy (ws >>. mpExpr .>> ws) (pchar ','))

    mpStructConstR.Value <- mpIdentifier .>>. mpStructExpr |>> MpStructConst |> mpPosition

    let mpInvokeVar =
        between (str_ws "(") (pstring ")") (sepBy (ws >>. mpExpr .>> ws) (pchar ','))

    mpInvokeR.Value <- pipe2 mpSetGet mpInvokeVar (fun x y -> MpInvoke(x, y)) |> mpPosition

    let mpReservedFuncIdentifier1: Parser<string, unit> =
        choice (reservedFunctions1 |> List.map pstring)

    let mpReservedFunc1 =
        (pipe2 mpReservedFuncIdentifier1 (between (str_ws "(") (pstring ")") (ws >>. mpExpr .>> ws)) (fun x y ->
            MpReservedFunc1(x, y)))
        |> mpPosition

    let mpReservedFuncIdentifier0: Parser<string, unit> =
        choice (reservedFunctions0 |> List.map pstring)

    let mpReservedFunc0 =
        (pipe2 mpReservedFuncIdentifier0 (between (str_ws "(") (pstring ")") ws) (fun x _ -> MpReservedFunc0 x))
        |> mpPosition

    mpReservedFuncR.Value <- mpReservedFunc0 <|> mpReservedFunc1

    let mpAssignAdd =
        pipe3 mpSetGet (ws >>. (str_ws "+") .>>. (str_ws "=")) mpExpr (fun (id, p) _ e ->
            MpAssign(Set((id, p), (MpArithmetic((id, p), MpAdd, e), p))))

    let mpAssignSubtract =
        pipe3 mpSetGet (ws >>. (str_ws "-") .>>. (str_ws "=")) mpExpr (fun (id, p) _ e ->
            MpAssign(Set((id, p), (MpArithmetic((id, p), MpSubtract, e), p))))

    let mpAssignMultiply =
        pipe3 mpSetGet (ws >>. (str_ws "*") .>>. (str_ws "=")) mpExpr (fun (id, p) _ e ->
            MpAssign(Set((id, p), (MpArithmetic((id, p), MpMultiply, e), p))))

    let mpAssignDivide =
        pipe3 mpSetGet (ws >>. (str_ws "/") .>>. (str_ws "=")) mpExpr (fun (id, p) _ e ->
            MpAssign(Set((id, p), (MpArithmetic((id, p), MpDivide, e), p))))

    let mpAssignRest =
        pipe3 mpSetGet (ws >>. (str_ws "%") .>>. (str_ws "=")) mpExpr (fun (id, p) _ e ->
            MpAssign(Set((id, p), (MpArithmetic((id, p), MpRest, e), p))))

    let mpAssignArithmeticAnd =
        pipe3 mpSetGet (ws >>. (str_ws "&") .>>. (str_ws "=")) mpExpr (fun (id, p) _ e ->
            MpAssign(Set((id, p), (MpArithmetic((id, p), MpArithmeticAnd, e), p))))

    let mpAssignArithmeticOr =
        pipe3 mpSetGet (ws >>. (str_ws "|") .>>. (str_ws "=")) mpExpr (fun (id, p) _ e ->
            MpAssign(Set((id, p), (MpArithmetic((id, p), MpArithmeticOr, e), p))))

    let mpAssignArithmeticXor =
        pipe3 mpSetGet (ws >>. (str_ws "^") .>>. (str_ws "=")) mpExpr (fun (id, p) _ e ->
            MpAssign(Set((id, p), (MpArithmetic((id, p), MpArithmeticXor, e), p))))

    let mpAssignAnd =
        pipe3 mpSetGet (ws >>. (str_ws "&&") .>>. (str_ws "=")) mpExpr (fun (id, p) _ e ->
            MpAssign(Set((id, p), (MpLogical((id, p), MpAnd, e), p))))

    let mpAssignOr =
        pipe3 mpSetGet (ws >>. (str_ws "||") .>>. (str_ws "=")) mpExpr (fun (id, p) _ e ->
            MpAssign(Set((id, p), (MpLogical((id, p), MpOr, e), p))))

    let mpAssignXor =
        pipe3 mpSetGet (ws >>. (str_ws "^^") .>>. (str_ws "=")) mpExpr (fun (id, p) _ e ->
            MpAssign(Set((id, p), (MpLogical((id, p), MpXor, e), p))))

    let mpAssignV =
        pipe3 mpSetGet (ws >>. (str_ws "=")) mpExpr (fun id _ e -> MpAssign(Set(id, e)))

    let mpAssign =
        [ mpAssignAnd
          mpAssignOr
          mpAssignXor
          mpAssignArithmeticAnd
          mpAssignArithmeticOr
          mpAssignArithmeticXor
          mpAssignAdd
          mpAssignSubtract
          mpAssignMultiply
          mpAssignDivide
          mpAssignRest
          mpAssignV ]
        |> List.map attempt
        |> choice

    let mpExprInstr = mpExpr |>> MpExpr

    let mpRange3 =
        pipe5 mpArithmetic (str_ws ",") mpArithmetic (str_ws ",") mpArithmetic (fun x _ y _ z -> (x, y, z))

    let mpRange2 =
        pipe3 mpArithmetic (str_ws ",") mpArithmetic (fun (x, p) _ y -> (x, p), y, (MpLiteral(MpInt 1), p))

    let mpRange1 =
        mpArithmetic
        |>> (fun (x, p) -> ((MpLiteral(MpInt 0), p), (x, p), (MpLiteral(MpInt 1), p)))

    let mpRange = attempt mpRange3 <|> attempt mpRange2 <|> attempt mpRange1

    let mpWhile =
        pipe5 getPosition (str_ws "while") (str_ws "(") mpLogical (str_ws ")") (fun p _ _ e _ -> MpWhile(e, p))

    let mpFor: Parser<instruction, unit> =
        pipe5 getPosition (str_ws "for") mpIdentifier_ws (str_ws "in") mpRange (fun p _ s _ (x, y, z) ->
            MpFor(s, x, y, z, p))

    let mpIf =
        pipe5 getPosition (str_ws "if") (str_ws "(") mpLogical (str_ws ")") (fun p _ _ e _ -> MpIf(e, p))

    let mpElIf =
        pipe5 getPosition (str_ws "elif") (str_ws "(") mpLogical (str_ws ")") (fun p _ _ e _ -> MpElIf(e, p))

    let mpElse = (getPosition .>>. pstring "else") |>> (fun (p, _) -> MpElse p)

    let mpStart: Parser<instruction, unit> = pstring "{" |>> (fun _ -> MpStart)

    let mpEnd: Parser<instruction, unit> = pstring "}" |>> (fun _ -> MpEnd)

    let mpFuncVar =
        between
            (str_ws "(")
            (pstring ")")
            (sepBy ((ws >>. getPosition .>>. mpIdentifier .>> ws) |>> (fun (x, y) -> (y, x))) (pchar ','))

    let mpFunc =
        pipe4 getPosition (str_ws "func") mpIdentifier mpFuncVar (fun p _ x y -> MpFunc(x, y, p))

    let mpStructProp =
        between (str_wsl "{") (pstring "}") (sepBy (wsl >>. mpIdentifier .>> wsl) (pchar ','))

    let mpStruct =
        pipe4 getPosition (str_ws "struct") mpIdentifier mpStructProp (fun p _ x y -> MpStruct(x, y, p))


    let mpReturnValue = pipe2 (str_ws "return") mpExpr (fun _ -> MpReturn)

    let mpReturnVoid =
        getPosition .>>. pstring "return"
        |>> (fun (p, _) -> MpReturn(MpLiteral MpNull, p))

    let mpReturn = attempt mpReturnValue <|> mpReturnVoid

    let mpBreak: Parser<instruction, unit> =
        getPosition .>>. pstring "break" |>> (fun (x, _) -> MpBreak x)

    let mpInstruct =
        [ mpAssign; mpExprInstr; mpReturn; mpBreak ] |> List.map attempt |> choice

    let mpBlockInstruct =
        [ mpWhile; mpFor; mpStart; mpEnd; mpIf; mpElIf; mpElse; mpFunc; mpStruct ]
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
