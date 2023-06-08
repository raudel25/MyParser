namespace MyParser

open System.Collections.Generic
open FParsec

type identifier = string
type index = int

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

and VarLookup = Dictionary<identifier, value>

and implType =
    | Static of identifier list * identifier list * instruction[]
    | Self of identifier list * identifier list * instruction[]

and FunctionsImpl=Dictionary<identifier,implType>

and FunctionsLookup = Dictionary<identifier, identifier list * identifier list * instruction[]>

and ClassLookup = Dictionary<identifier, identifier list * FunctionsImpl>

and State = VarLookup * FunctionsLookup * ClassLookup

and ProgramState = VarLookup * FunctionsLookup * ClassLookup * instruction[]

and value =
    | MpNull
    | MpBool of bool
    | MpInt of int
    | MpDouble of double
    | MpString of string
    | MpChar of char
    | MpTupleValue of value[]
    | MpArrayValue of value[]
    | MpFuncStaticValue of identifier * identifier list * identifier list * instruction[]
    | MpFuncSelfValue of identifier * identifier list * identifier list * instruction[] * value
    | MpObjectValue of identifier * Dictionary<identifier, value> * FunctionsImpl
    | MpClassValue of identifier * identifier list * FunctionsImpl

and exprT =
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
    | MpClassConst of expr * expr list
    | MpLambda of (identifier * Position) list * instruction[]
    | MpSelf

and expr = exprT * Position

and property =
    | MpIndexA of expr
    | MpProperty of identifier * Position
    | MpIndexT of int * Position

and assign = Set of expr * expr

and instruction =
    | MpClass of identifier * identifier list * Position
    | MpFunc of identifier * (identifier * Position) list * Position * instruction[]
    | MpAssign of assign
    | MpExpr of expr
    | MpFor of identifier * expr * expr * expr * instruction[] * Position
    | MpIf of expr * instruction[]
    | MpElIf of expr * instruction[] * expr * instruction[]
    | MpElse of expr * instruction[] * instruction[]
    | MpElIfElse of expr * instruction[] * expr * instruction[] * instruction[]
    | MpWhile of expr * instruction[]
    | MpReturn of expr
    | MpBreak of uint8 * Position
    | MpComment
    | MpImpl of identifier * instructionImpl list * Position
    | MpImplDeriving of identifier * Position * identifier * Position * instructionImpl list 

and instructionImpl =
    | MpImplFunc of identifier * (identifier * Position) list * Position * instruction[]
    | MpImplSelf of identifier * (identifier * Position) list * Position * instruction[]



