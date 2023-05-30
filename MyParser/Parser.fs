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
    | MpEndWhile
    | MpSub of identifier
    | MpEndSub
    | MpGoSub of identifier
    | MpLabel of label
    | MpGoto of label

open System
open FParsec

module Parser =

    let (>>%) p x = p |>> (fun _ -> x)

    let mpNull: Parser<value, unit> = pstring "null" >>% MpNull <?> "null"

    let mpBool: Parser<value, obj> =
        let mpTrue = pstring "true" >>% MpBool true
        let mpFalse = pstring "false" >>% MpBool false

        mpTrue <|> mpFalse <?> "bool"

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

    let mpString = quotedString <?> "quoted string" |>> MpString

    let mpNumLiteral: Parser<expr, unit> =
        let numberFormat = NumberLiteralOptions.AllowFraction

        numberLiteral numberFormat "number"
        |>> fun nl ->
                if nl.IsInteger then
                    MpLiteral(MpInt(int nl.String))
                else
                    MpLiteral(MpFloat(float nl.String))

    let ws: Parser<unit, unit> =
        skipManySatisfy (fun c -> c = ' ' || c = '\t' || c = '\r')

    let str_ws s = pstring s .>> ws
    let str_ws1 s = pstring s .>> spaces1

    let mpIdentifier: Parser<string, unit> =
        let isIdentifierFirstChar c = isLetter c || c = '_'
        let isIdentifierChar c = isLetter c || isDigit c || c = '_'
        many1Satisfy2L isIdentifierFirstChar isIdentifierChar "identifier"

    let mpIdentifier_ws = mpIdentifier .>> ws
    let mpVar = mpIdentifier |>> MpVar
// let pinvoke, pinvokeimpl = createParserForwardedToRef ()
// let pfunc = pinvoke |>> (fun x -> Func<_>(x))
//
// let plocation, plocationimpl = createParserForwardedToRef ()
// let pgetat = plocation |>> (fun loc -> GetAt(loc))
//
// let pvalue =
//     choice [
//         mpNumLiteral; mpString
//         attempt mpVar
//     ]
// type Assoc = Associativity
//
// let oppa = new OperatorPrecedenceParser<expr,unit,unit>()
// let parithmetic = oppa.ExpressionParser
// let terma = (pvalue .>> ws) <|> between (str_ws "(") (str_ws ")") parithmetic
// oppa.TermParser <- terma
// oppa.AddOperator(InfixOperator("+", ws, 1, Assoc.Left, fun x y -> Arithmetic(x, Add, y)))
// oppa.AddOperator(InfixOperator("-", ws, 1, Assoc.Left, fun x y -> Arithmetic(x, Subtract, y)))
// oppa.AddOperator(InfixOperator("*", ws, 2, Assoc.Left, fun x y -> Arithmetic(x, Multiply, y)))
// oppa.AddOperator(InfixOperator("/", ws, 2, Assoc.Left, fun x y -> Arithmetic(x, Divide, y)))
// oppa.AddOperator(PrefixOperator("-", ws, 2, true, fun x -> Neg(x)))
