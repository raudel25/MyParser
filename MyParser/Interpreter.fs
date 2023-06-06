namespace MyParser

open System
open System.Collections.Generic
open FParsec
open Microsoft.FSharp.Core
open MyParser.LibraryFunc

module Interpreter =

    let fromObj (pos: Position) (x: obj) =
        match x with
        | :? bool as x -> MpBool x
        | :? int as x -> MpInt x
        | :? double as x -> MpDouble x
        | :? string as x -> MpString x
        | :? char as x -> MpChar x
        | null -> MpNull
        | x -> raise (Exception(error pos (x.ToString())))



    let toInt (pos: Position) =
        let f x =
            match x with
            | MpInt x -> x
            | MpDouble x -> int x
            | MpString x -> int x
            | MpNull -> 0
            | _ -> raise (Exception(error pos "Cannot convert to int"))

        f

    let toBool (pos: Position) =
        let f x =
            match x with
            | MpBool x -> x
            | _ -> raise (Exception(error pos "Cannot convert to bool"))

        f

    let (|AsDoubles|_|) =
        function
        | MpDouble l, MpDouble r -> Some(l, r)
        | MpInt l, MpDouble r -> Some(double l, r)
        | MpDouble l, MpInt r -> Some(l, double r)
        | _, _ -> None

    let compare (pos: Position) lhs rhs =
        match lhs, rhs with
        | MpBool l, MpBool r -> l.CompareTo(r)
        | MpInt l, MpInt r -> l.CompareTo(r)
        | AsDoubles (l, r) -> l.CompareTo(r)
        | MpString l, MpString r -> l.CompareTo(r)
        | MpChar l, MpChar r -> l.CompareTo(r)
        | _ -> raise (Exception(error pos $"%A{lhs} %A{rhs}"))

    type VarLookup = Dictionary<identifier, value>
    type FunctionsLookup = Dictionary<identifier, identifier * identifier list * List<identifier> * instruction[]>
    type StructLookup = Dictionary<identifier, identifier list>
    type State = VarLookup * FunctionsLookup * StructLookup
    type ProgramState = VarLookup * FunctionsLookup * StructLookup * instruction[]

    let rec eval (state: ProgramState) (expr: expr) =
        let (vars: VarLookup, functions: FunctionsLookup, structs: StructLookup, _: instruction[]) =
            state

        let expr, pos = expr

        match expr with
        | MpLiteral x -> x
        | MpVar identifier ->
            if functions.ContainsKey(identifier) then
                let s, x, y, z = functions[identifier]
                MpFuncValue(s, x, y, z)
            else
                if not (vars.ContainsKey(identifier)) then
                    raise (Exception(error pos "Variable does not exist"))

                vars[identifier]
        | MpArrayL a -> MpArrayValue(Array.map (eval state) a)
        | MpArrayD (a, b) ->
            let a = eval state a
            let _, pos = b
            let b = eval state b

            match b with
            | MpInt b -> MpArrayValue(Array.create b a)
            | _ -> raise (Exception(error pos "Cannot convert to int"))
        | MpTuple a -> MpTupleValue(Array.map (eval state) a)
        | MpIdentProp (identifier, indices) ->
            if not (vars.ContainsKey(identifier)) then
                raise (Exception(error pos "Variable does not exist"))

            let value = vars[identifier]

            getProp state value indices 0
        | MpSlice (identifier, start, stop) ->
            let value = eval state identifier
            let start, stop = (eval state start, eval state stop)

            match (value, start, stop) with
            | MpArrayValue v, MpInt start, MpInt stop -> MpArrayValue v[start .. stop - 1]
            | MpString s, MpInt start, MpInt stop -> MpString s[start .. stop - 1]
            | _ -> raise (Exception(error pos "Slice is not supported"))

        | MpNeg x -> arithmetic pos (eval state x) MpMultiply (MpInt(-1))
        | MpArithmetic (l, op, r) -> arithmetic pos (eval state l) op (eval state r)
        | MpComparison (l, op, r) -> comparison pos (eval state l) op (eval state r)
        | MpLogical (l, op, r) -> logical pos (eval state l) op (eval state r)
        | MpInvoke (s, expr) ->
            let s = eval state s

            match s with
            | MpFuncValue (_, identifiers, globals, block) ->
                let variables = VarLookup()
                let newFunctions = FunctionsLookup()

                if identifiers.Length <> expr.Length then
                    raise (Exception(error pos "Function does not have correct parameters"))

                for i in 0 .. identifiers.Length - 1 do
                    variables[identifiers[i]] <- (eval state expr[i])

                for var in globals do
                    variables[var] <- vars[var]

                for f in functions do
                    newFunctions[f.Key] <- f.Value

                let aux = mpRunAux (ProgramState(variables, newFunctions, structs, block))

                for var in globals do
                    vars[var] <- variables[var]

                aux
            | _ -> raise (Exception(error pos "Expression is not function"))
        | MpReservedFunc0 s -> funcLib0 s
        | MpReservedFunc1 (s, expr) ->
            let _, pos = expr
            funcLib1 pos s (eval state expr)
        | MpTernary (cond, e1, e2) ->
            let cond = eval state cond

            if (toBool pos cond) then eval state e1 else eval state e2

        | MpStructConst (s, props) ->
            if not (structs.ContainsKey(s)) then
                raise (Exception(error pos "Struct does not exist"))

            let listProps = structs[s]

            if listProps.Length <> props.Length then
                raise (Exception(error pos "Struct does not have correct parameters"))

            let q = Dictionary<identifier, value>()

            for i in 0 .. listProps.Length - 1 do
                q[listProps[i]] <- (eval state props[i])

            MpStructValue(s, q)


    and comparison (pos: Position) lhs op rhs =
        let x = compare pos lhs rhs

        match op with
        | MpEq -> x = 0
        | MpNe -> x <> 0
        | MpLt -> x < 0
        | MpGt -> x > 0
        | MpLe -> x <= 0
        | MpGe -> x >= 0
        |> fromObj pos

    and arithmetic (pos: Position) lhs op rhs =
        match op, (lhs, rhs) with
        | MpAdd, (MpInt l, MpInt r) -> MpInt(l + r)
        | MpAdd, (MpChar l, MpChar r) -> MpChar(l + r)
        | MpAdd, (MpString l, x) -> MpString(l + toStr x)
        | MpAdd, (x, MpString r) -> MpString(toStr x + r)
        | MpAdd, AsDoubles (l, r) -> MpDouble(l + r)
        | MpSubtract, (MpInt l, MpInt r) -> MpInt(l - r)
        | MpSubtract, (MpChar l, MpChar r) -> MpChar(l - r)
        | MpSubtract, AsDoubles (l, r) -> MpDouble(l - r)
        | MpMultiply, (MpInt l, MpInt r) -> MpInt(l * r)
        | MpMultiply, AsDoubles (l, r) -> MpDouble(l * r)
        | MpDivide, (MpInt l, MpInt r) -> MpInt(l - r)
        | MpDivide, AsDoubles (l, r) -> MpDouble(l - r)
        | MpRest, (MpInt l, MpInt r) -> MpInt(l % r)
        | MpArithmeticAnd, (MpInt l, MpInt r) -> MpInt(l &&& r)
        | MpArithmeticOr, (MpInt l, MpInt r) -> MpInt(l ||| r)
        | MpArithmeticXor, (MpInt l, MpInt r) -> MpInt(l ^^^ r)
        | _ -> raise (Exception(error pos "Arithmetic operation is not supported"))

    and logical (pos: Position) lhs op rhs =
        match op, lhs, rhs with
        | MpAnd, MpBool l, MpBool r -> MpBool(l && r)
        | MpOr, MpBool l, MpBool r -> MpBool(l || r)
        | MpXor, MpBool l, MpBool r -> MpBool((not l && r) || (not r && l))
        | _, _, _ -> raise (Exception(error pos "Logical operation is not supported"))

    and getProp (state: ProgramState) (value: value) (indices: property list) ind =
        let getValue v =
            if ind = indices.Length - 1 then
                v
            else
                getProp state v indices (ind + 1)

        let getArray pos (v: value[]) index =
            if index < 0 || index >= v.Length then
                raise (Exception(error pos "Index out range"))

            getValue v[index]

        match indices[ind] with
        | MpIndexA index ->
            let _, pos = index
            let index = toInt pos (eval state index)

            match value with
            | MpArrayValue v -> getArray pos v index
            | MpString v ->
                if index < 0 || index >= v.Length then
                    raise (Exception(error pos "Index out range"))

                if ind = 0 && indices.Length = 1 then
                    MpChar v[index]
                else
                    raise (Exception(error pos "The indexed value is not correct"))
            | _ -> raise (Exception(error pos "The indexed value is not correct"))
        | MpIndexT (index, pos) ->
            match value with
            | MpTupleValue v -> getArray pos v index
            | _ -> raise (Exception(error pos "The property is not correct"))
        | MpProperty (prop, pos) ->
            match value with
            | MpStructValue (_, v) ->
                if not (v.ContainsKey(prop)) then
                    raise (Exception(error pos "The property is not correct"))

                getValue v[prop]
            | _ -> raise (Exception(error pos "The property is not correct"))


    and setProp (state: ProgramState) (value: value) (indices: property list) ind (e: value) =
        let setArray pos (v: value[]) index =
            if index < 0 || index >= v.Length then
                raise (Exception(error pos "Index out range"))

            if ind = indices.Length - 1 then
                v[index] <- e
            else
                setProp state v[index] indices (ind + 1) e

        match indices[ind] with
        | MpIndexA index ->
            let _, pos = index
            let index = toInt pos (eval state index)

            match value with
            | MpArrayValue v -> setArray pos v index
            | _ -> raise (Exception(error pos "The indexed set is not correct"))
        | MpIndexT (_, pos) -> raise (Exception(error pos "The indexed set is not correct"))
        | MpProperty (prop, pos) ->
            match value with
            | MpStructValue (_, v) ->
                if not (v.ContainsKey(prop)) then
                    raise (Exception(error pos "The property is not correct"))

                if ind = indices.Length - 1 then
                    v[prop] <- e
                else
                    setProp state v[prop] indices (ind + 1) e
            | _ -> raise (Exception(error pos "The property is not correct"))


    and mpRunAux (state: ProgramState) =
        let (variables: VarLookup, functions: FunctionsLookup, structs: StructLookup, program: instruction[]) =
            state

        let mutable index = 0
        let mutable valueReturn = MpNull
        let mutable stopFunc = false
        let evalAux = eval state

        let assign (value: assign) =
            match value with
            | Set (identifier, expr) ->
                let identifier, pos = identifier

                match identifier with
                | MpVar identifier ->
                    if functions.ContainsKey(identifier) || structs.ContainsKey(identifier) then
                        raise (Exception(error pos "There are two terms with the same name"))

                    variables[identifier] <- evalAux expr
                | MpIdentProp (identifier, indices) ->
                    let value = variables[identifier]

                    setProp state value indices 0 (evalAux expr)

                | _ -> ()

        let sameName pos identifier =
            if
                functions.ContainsKey(identifier)
                || variables.ContainsKey(identifier)
                || structs.ContainsKey(identifier)
            then
                raise (Exception(error pos "There are two terms with the same name"))


        let rec step instruction whileOrFor =
            match instruction with
            | MpAssign set ->
                assign set
                false
            | MpIf (cond, block) ->
                let _, pos = cond

                if toBool pos (evalAux cond) then
                    executeBlock block whileOrFor
                else
                    false
            | MpElse (cond, blockIf, blockElse) ->
                let _, pos = cond

                if toBool pos (evalAux cond) then
                    executeBlock blockIf whileOrFor
                else
                    executeBlock blockElse whileOrFor
            | MpElIf (condIf, blockIf, condElIf, blockElIf) ->
                let _, posIf = condIf
                let _, posElIf = condElIf

                if toBool posIf (evalAux condIf) then
                    executeBlock blockIf whileOrFor
                elif toBool posElIf (evalAux condElIf) then
                    executeBlock blockElIf whileOrFor
                else
                    false
            | MpElIfElse (condIf, blockIf, condElIf, blockElIf, blockElse) ->
                let _, posIf = condIf
                let _, posElIf = condElIf

                if toBool posIf (evalAux condIf) then
                    executeBlock blockIf whileOrFor
                elif toBool posElIf (evalAux condElIf) then
                    executeBlock blockElIf whileOrFor
                else
                    executeBlock blockElse whileOrFor
            | MpFor (identifier, initE, stopE, stepE, block, pos) ->
                let toIntAux expr =
                    let value = (evalAux expr)
                    let _, pos = expr

                    match value with
                    | MpInt n -> n
                    | _ -> raise (Exception(error pos "Cannot convert to int"))

                let _, stop, step = (toIntAux initE, toIntAux stopE, toIntAux stepE)

                let _ = assign (Set((MpVar(identifier), pos), initE))

                let mutable q = true

                while toInt pos variables[identifier] < stop && q do
                    if executeBlock block true then
                        q <- false

                    variables[identifier] <- arithmetic pos variables[identifier] MpAdd (MpInt step)

                false

            | MpWhile (condition, block) ->
                let _, pos = condition

                let mutable q = true

                while (evalAux condition |> toBool pos) && q do
                    if executeBlock block true then
                        q <- false

                false
            | MpExpr x ->
                let _ = evalAux x
                false

            | MpFunc (identifier, vars, pos, block) ->
                let _ = sameName pos identifier

                let globals = List<identifier>()

                let contains var =
                    let mutable q = false

                    for i in vars do
                        let i, _ = i
                        q <- q || i = var

                    q

                for i in variables do
                    if not (contains i.Key) then
                        globals.Add(i.Key)

                let newVars = List.map fst vars

                functions[identifier] <- (identifier, newVars, globals, block)

                for i in vars do
                    let i, p = i

                    if functions.ContainsKey(i) || structs.ContainsKey(i) then
                        raise (Exception(error p "There are two terms with the same name"))

                false
            | MpReturn expr ->
                valueReturn <- evalAux expr
                stopFunc <- true

                false

            | MpBreak pos ->
                if not whileOrFor then
                    raise (Exception(error pos "Incorrect instruction break"))

                true

            | MpStruct (identifier, vars, pos) ->
                let _ = sameName pos identifier

                structs[identifier] <- vars

                false
            | MpComment -> false

        and executeBlock block whileOrFor =
            let mutable i = 0
            let mutable q = true

            while q && i < block.Length do
                if step block[i] whileOrFor || stopFunc then
                    q <- false

                i <- i + 1

            not q

        while index < program.Length && not stopFunc do
            let _ = step program[index] false

            index <- index + 1

        valueReturn

    let mpState =
        let variables = VarLookup()
        let functions = FunctionsLookup()
        let structs = StructLookup()

        State(variables, functions, structs)

    let mpInteractive (state: State) (program: instruction[]) start =
        let variables, functions, structs = state

        try
            let _ = mpRunAux (ProgramState(variables, functions, structs, program))

            program.Length
        with ex ->
            let s = ex.Message.Split('\n')

            if s.Length > 1 && s[1] = "Not find end block instruction }" then
                start
            else
                raise (Exception(ex.Message))

    let mpRun (program: instruction[]) =
        let variables = VarLookup()
        let functions = FunctionsLookup()
        let structs = StructLookup()

        mpRunAux (ProgramState(variables, functions, structs, program))
