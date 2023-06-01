namespace MyParser

open System
open System.Collections.Generic
open Microsoft.FSharp.Core
open MyParser.LibraryFunc

module Interpreter =
    
    let error (s:string)=
        $"Error in execution: {s}"

    let fromObj (x: obj) =
        match x with
        | :? bool as x -> MpBool x
        | :? int as x -> MpInt x
        | :? double as x -> MpDouble x
        | :? string as x -> MpString x
        | :? char as x -> MpChar x
        | null -> MpNull
        | x -> raise (Exception(error(x.ToString())))



    let toInt =
        let f x =
            match x with
            | MpInt x -> x
            | MpDouble x -> int x
            | MpString x -> int x
            | MpNull -> 0
            | _ -> raise (Exception(error "Cannot convert to int"))

        f

    let toBool =
        let f x =
            match x with
            | MpBool x -> x
            | _ -> raise (Exception(error "Cannot convert to bool"))

        f

    let (|AsDoubles|_|) =
        function
        | MpDouble l, MpDouble r -> Some(l, r)
        | MpInt l, MpDouble r -> Some(double l, r)
        | MpDouble l, MpInt r -> Some(l, double r)
        | _, _ -> None

    let compare lhs rhs =
        match lhs, rhs with
        | MpBool l, MpBool r -> l.CompareTo(r)
        | MpInt l, MpInt r -> l.CompareTo(r)
        | AsDoubles (l, r) -> l.CompareTo(r)
        | MpString l, MpString r -> l.CompareTo(r)
        | MpChar l, MpChar r -> l.CompareTo(r)
        | _ -> raise (Exception (error $"%A{lhs} %A{rhs}"))

    type VarLookup = Dictionary<identifier, value>
    type FunctionsLookup = Dictionary<identifier, identifier list * List<identifier> * int * int>

    type State = VarLookup * FunctionsLookup
    type ProgramState = VarLookup * FunctionsLookup * instruction[]

    let rec eval (state: ProgramState) (expr: expr) =
        let (vars: VarLookup, functions: FunctionsLookup, program: instruction[]) = state

        match expr with
        | MpLiteral x -> x
        | MpVar identifier ->
            if functions.ContainsKey(identifier) then
                MpFuncValue identifier
            else
                if not (vars.ContainsKey(identifier)) then
                    raise (Exception(error "Variable does not exist"))

                vars[identifier]
        | MpArrayL a ->
            let newA = Array.create a.Length MpNull

            for i in 0 .. (a.Length - 1) do
                let result = eval state a[i]
                newA[i] <- result

            MpArrayValue newA
        | MpArrayD (a, b) ->
            let a = eval state a
            let b = eval state b

            match b with
            | MpInt b -> MpArrayValue(Array.create b a)
            | _ -> raise (Exception(error "Cannot convert to int"))
        | MpIndex (identifier, indices) ->
            if not (vars.ContainsKey(identifier)) then
                raise (Exception(error "Variable does not exist"))

            let value = vars[identifier]

            getIndices state value indices 0
        | MpSlice (identifier, start, stop) ->
            if not (vars.ContainsKey(identifier)) then
                raise (Exception(error "Variable does not exist"))

            let start, stop = (eval state start, eval state stop)

            let value = vars[identifier]

            match (value, start, stop) with
            | MpArrayValue v, MpInt start, MpInt stop -> MpArrayValue v[start .. stop - 1]
            | MpString s, MpInt start, MpInt stop -> MpString s[start .. stop - 1]
            | _ -> raise (Exception(error "Slice is not supported"))

        | MpNeg x -> arithmetic (eval state x) MpMultiply (MpInt(-1))
        | MpArithmetic (l, op, r) -> arithmetic (eval state l) op (eval state r)
        | MpComparison (l, op, r) -> comparison (eval state l) op (eval state r)
        | MpLogical (l, op, r) -> logical (eval state l) op (eval state r)
        | MpInvoke (s, expr) ->
            let mutable func = s

            if vars.ContainsKey(s) then
                match vars[s] with
                | MpFuncValue q -> func <- q
                | _ -> raise (Exception(error "Variable is not function"))

            if not (functions.ContainsKey(func)) then
                raise (Exception(error "Function does not exist"))

            let variables = VarLookup()
            let identifiers, globals, start, stop = functions[func]

            if identifiers.Length <> expr.Length then
                raise (Exception(error "Function does not have correct parameters"))

            for i in 0 .. identifiers.Length - 1 do
                variables[identifiers[i]] <- (eval state expr[i])

            for var in globals do
                variables[var] <- vars[var]

            let aux = mpRunAux (ProgramState(variables, functions, program)) start stop

            for var in globals do
                vars[var] <- variables[var]

            aux
        | MpReservedFunc0 s -> funcLib0 s
        | MpReservedFunc1 (s, expr) -> funcLib1 s (eval state expr)

    and comparison lhs op rhs =
        let x = compare lhs rhs

        match op with
        | MpEq -> x = 0
        | MpNe -> x <> 0
        | MpLt -> x < 0
        | MpGt -> x > 0
        | MpLe -> x <= 0
        | MpGe -> x >= 0
        |> fromObj

    and arithmetic lhs op rhs =
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
        | _ -> raise (Exception(error "Arithmetic operation is not supported"))

    and logical lhs op rhs =
        match op, lhs, rhs with
        | MpAnd, MpBool l, MpBool r -> MpBool(l && r)
        | MpOr, MpBool l, MpBool r -> MpBool(l || r)
        | MpXor, MpBool l, MpBool r -> MpBool((not l && r) || (not r && l))
        | _, _, _ -> raise (Exception(error "Logical operation is not supported"))

    and getIndices (state: ProgramState) (value: value) (indices: expr list) ind =
        let index = toInt (eval state indices[ind])

        match value with
        | MpArrayValue v ->
            if index < 0 || index >= v.Length then
                raise (IndexOutOfRangeException())

            if ind = indices.Length - 1 then
                v[index]
            else
                getIndices state v[index] indices (ind + 1)
        | MpString v ->
            if index < 0 || index >= v.Length then
                raise (IndexOutOfRangeException())

            if ind = 0 && indices.Length = 1 then
                MpChar v[index]
            else
                raise (Exception(error "The indexed value is not correct"))
        | _ -> raise (Exception(error "The indexed value is not correct"))

    and setIndices (state: ProgramState) (value: value) (indices: expr list) ind (e: value) =
        let index = toInt (eval state indices[ind])

        match value with
        | MpArrayValue v ->
            if index < 0 || index >= v.Length then
                raise (IndexOutOfRangeException())

            if ind = indices.Length - 1 then
                v[index] <- e
            else
                setIndices state v[index] indices (ind + 1) e

        | _ -> raise (Exception(error "The indexed value is not correct"))

    and mpRunAux (state: ProgramState) pi pe =
        let (variables: VarLookup, functions: FunctionsLookup, program: instruction[]) =
            state

        let mutable pi = pi
        let mutable valueReturn = MpNull
        let loops = Stack<index * index>()
        let evalAux = eval state

        let assign (value: assign) =
            match value with
            | Set (identifier, expr) ->
                if functions.ContainsKey(identifier) then
                    raise (Exception(error "There are two terms with the same name"))

                variables[identifier] <- evalAux expr
            | SetE (identifier, expr) ->
                match identifier with
                | MpIndex (identifier, indices) ->
                    let value = variables[identifier]

                    setIndices state value indices 0 (evalAux expr)

                | _ -> ()

        let initBlock instruction =
            match instruction with
            | MpStart -> true
            | _ -> false

        let endBlock instruction =
            match instruction with
            | MpEnd -> true
            | _ -> false

        let rec findEndBlock ind cant =
            if ind >= program.Length then
                raise (Exception(error "Excepted }"))

            let mutable newCant = cant

            if initBlock program[ind] then
                newCant <- cant + 1

            if endBlock program[ind] then
                newCant <- cant - 1

            if newCant = 0 then ind else findEndBlock (ind + 1) newCant

        let rec findStartBlock ind cant =
            if ind < 1 then
                raise (Exception(error "Excepted {"))

            let mutable newCant = cant

            if initBlock program[ind] then
                newCant <- cant - 1

            if endBlock program[ind] then
                newCant <- cant + 1

            if newCant = 0 then
                ind
            else
                findStartBlock (ind - 1) newCant


        let step () =
            let instruction = program[pi]

            match instruction with
            | MpAssign set -> assign set
            | MpIf cond ->
                let condition = toBool (evalAux cond)

                if not condition then
                    let index = findEndBlock (pi + 1) 0
                    pi <- index

            | MpElse ->
                let indexStart = findStartBlock (pi - 1) 0

                let execute cond =
                    let condition = toBool (evalAux cond)

                    if condition then
                        let index = findEndBlock (pi + 1) 0
                        pi <- index

                match program[indexStart - 1] with
                | MpIf cond -> execute cond
                | MpElIf cond -> execute cond
                | _ -> raise (NotSupportedException("Excepted else"))

            | MpElIf condEl ->
                let indexStart = findStartBlock (pi - 1) 0

                let execute condIf =
                    let conditionIf = toBool (evalAux condIf)
                    let conditionEl = toBool (evalAux condEl)

                    if (conditionIf || not conditionEl) then
                        let indexEnd = findEndBlock (pi + 1) 0
                        pi <- indexEnd

                match program[indexStart - 1] with
                | MpIf condIf -> execute condIf
                | MpElIf condIf -> execute condIf
                | _ -> raise (NotSupportedException("Excepted elif"))

            | MpFor (identifier, init, stop, step) ->
                let init, stop, step = (evalAux init, evalAux stop, evalAux step)

                match (init, stop, step) with
                | MpInt init, MpInt stop, MpInt step ->
                    let mutable index = 0

                    let execute () =
                        assign (Set(identifier, MpLiteral(MpInt init)))
                        index <- findEndBlock (pi + 1) 0
                        loops.Push((pi, index))

                    if loops.Count = 0 then
                        execute ()
                    else
                        let piAux, indexAux = loops.Peek()

                        if piAux <> pi then
                            execute ()
                        else
                            variables[identifier] <- arithmetic variables[identifier] MpAdd (MpInt step)
                            index <- indexAux

                    if toInt variables[identifier] >= stop then
                        let _ = loops.Pop()
                        pi <- index
                | _ -> raise (Exception(error "Cannot convert to int"))

            | MpWhile condition ->
                let mutable index = 0

                let execute () =
                    index <- findEndBlock (pi + 1) 0
                    loops.Push((pi, index))

                if loops.Count = 0 then
                    execute ()
                else
                    let piAux, indexAux = loops.Peek()

                    if piAux <> pi then execute () else index <- indexAux

                if evalAux condition |> toBool |> not then
                    let _ = loops.Pop()
                    pi <- index

            | MpStart -> ()
            | MpEnd -> ()
            | MpExpr x ->
                let _ = evalAux x
                ()

            | MpFunc (identifier, vars) ->
                if functions.ContainsKey(identifier) || variables.ContainsKey(identifier) then
                    raise (Exception(error "There are two terms with the same name"))

                let index = findEndBlock (pi + 1) 0
                let globals = List<identifier>()

                let contains var =
                    let mutable q = false

                    for i in vars do
                        q <- q || i = var

                    q

                for i in variables do
                    if not (contains i.Key) then
                        globals.Add(i.Key)

                functions[identifier] <- (vars, globals, pi + 1, index + 1)

                for i in vars do
                    if functions.ContainsKey(i) then
                        raise (Exception(error "There are two terms with the same name"))

                pi <- index

            | MpReturn expr ->
                valueReturn <- evalAux expr
                pi <- pe

            | MpBreak ->
                if loops.Count = 0 then
                    raise (Exception(error "Except break"))

                let _, index = loops.Peek()
                let _ = loops.Pop()
                pi <- index

        while pi < pe do
            step ()

            if loops.Count <> 0 then
                let piAux, index = loops.Peek()

                if index = pi then
                    pi <- piAux - 1

            pi <- pi + 1

        valueReturn

    let mpState =
        let variables = VarLookup()
        let func = FunctionsLookup()

        State(variables, func)

    let mpInteractive (state: State) (program: instruction[]) start =
        let variables, func = state

        try
            let _ = mpRunAux (ProgramState(variables, func, program)) start program.Length
            program.Length
        with
        | :? Exception as ex ->
            if ex.Message = "Excepted }" then
                start
            else
                raise (Exception(ex.Message))
        | _ -> raise (Exception())


    let mpRun (program: instruction[]) =
        let variables = VarLookup()
        let func = FunctionsLookup()

        mpRunAux (ProgramState(variables, func, program)) 0 program.Length
