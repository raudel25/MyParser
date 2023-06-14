namespace PySharp

open System.Collections.Generic
open FParsec

type identifier = string

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

and func =
    | Static of identifier list * identifier list * instruction[]
    | Self of identifier list * identifier list * instruction[]

and FunctionsLookup = Dictionary<identifier, func>

and ClassLookup = Dictionary<identifier, identifier list * ScopeClass>

and ModulesLookup = Module of  Dictionary<identifier,Scope>

and Scope = VarLookup * FunctionsLookup * ClassLookup * ModulesLookup

and ScopeClass=VarLookup * FunctionsLookup

and ProgramScope = Scope * instruction[]

and value =
    | MpNull
    | MpBool of bool
    | MpInt of int
    | MpDouble of double
    | MpString of string
    | MpChar of char
    | MpTupleValue of value[]
    | MpArrayValue of value[]
    | MpFuncStaticValue of identifier * identifier list * identifier list * instruction[] * ScopeClass
    | MpFuncSelfValue of identifier * identifier list * identifier list * instruction[] * ScopeClass * value
    | MpObjectValue of identifier * Dictionary<identifier, value> * ScopeClass
    | MpClassValue of identifier * identifier list * ScopeClass
    | MpModuleValue of identifier * Scope

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
    | MpLambda of identPos list * instruction[]
    | MpSelf

and expr = exprT * Position

and identPos = identifier * Position

and property =
    | MpIndexA of expr
    | MpProperty of identPos
    | MpIndexT of int * Position

and assign = Set of expr * expr

and instruction =
    | MpClass of identPos * identifier list
    | MpFuncStatic of identPos * identPos list * instruction[]
    | MpFuncSelf of identPos * identPos list * instruction[]
    | MpAssign of assign
    | MpExpr of expr
    | MpFor of identPos * expr * expr * expr * instruction[]
    | MpLoop of instruction[]
    | MpIf of expr * instruction[]
    | MpElIf of expr * instruction[] * expr * instruction[]
    | MpElse of expr * instruction[] * instruction[]
    | MpElIfElse of expr * instruction[] * expr * instruction[] * instruction[]
    | MpWhile of expr * instruction[]
    | MpReturn of expr
    | MpBreak of uint8 * Position
    | MpContinue of uint8 * Position
    | MpComment
    | MpImpl of identPos * instruction[]
    | MpImplDeriving of identPos * identPos * instruction[]
    | MpModule of identPos * instruction[]