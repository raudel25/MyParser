namespace MyParser

open System
open System.Collections.Generic
open Microsoft.FSharp.Core

module Interpreter =

    let fromObj (x: obj) =
        match x with
        | :? bool as x -> MpBool x
        | :? int as x -> MpInt x
        | :? double as x -> MpFloat x
        | :? string as x -> MpString x
        | null -> MpNull
        | x -> raise (NotSupportedException(x.ToString()))



    let toInt =
        let f x =
            match x with
            | MpInt x -> x
            | MpFloat x -> int x
            | MpString x -> int x
            | MpNull -> 0
            | _ -> raise (NotSupportedException("Cannot convert to int"))

        f

    let toBool =
        let f x =
            match x with
            | MpBool x -> x
            | _ -> raise (NotSupportedException("Cannot convert to bool"))

        f

    let (|AsFloats|_|) =
        function
        | MpFloat l, MpFloat r -> Some(l, r)
        | MpInt l, MpFloat r -> Some(float l, r)
        | MpFloat l, MpInt r -> Some(l, float r)
        | _, _ -> None

    let compare lhs rhs =
        match lhs, rhs with
        | MpBool l, MpBool r -> l.CompareTo(r)
        | MpInt l, MpInt r -> l.CompareTo(r)
        | AsFloats (l, r) -> l.CompareTo(r)
        | MpString l, MpString r -> l.CompareTo(r)
        | _ -> raise (NotSupportedException $"%A{lhs} %A{rhs}")

    type VarLookup = Dictionary<identifier, value>

    let rec eval state (expr: expr) =
        let (vars: VarLookup) = state

        match expr with
        | MpLiteral x -> x
        | MpVar identifier -> vars[identifier]
        | MpArray a ->
            let newA = Array.create a.Length MpNull

            for i in 0 .. (a.Length - 1) do
                let result = eval state a[i]
                newA[i] <- result

            MpArrayValue newA
        | MpIndex (identifier, indices) ->
            let value = vars[identifier]

            getIndices vars value indices 0

        | MpNeg x -> arithmetic (eval state x) MpMultiply (MpInt(-1))
        | MpArithmetic (l, op, r) -> arithmetic (eval state l) op (eval state r)
        | MpComparison (l, op, r) -> comparison (eval state l) op (eval state r)
        | MpLogical (l, op, r) -> logical (eval state l) op (eval state r)

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
        | MpAdd, AsFloats (l, r) -> MpFloat(l + r)
        | MpSubtract, (MpInt l, MpInt r) -> MpInt(l - r)
        | MpSubtract, AsFloats (l, r) -> MpFloat(l - r)
        | MpMultiply, (MpInt l, MpInt r) -> MpInt(l * r)
        | MpMultiply, AsFloats (l, r) -> MpFloat(l * r)
        | MpDivide, (MpInt l, MpInt r) -> MpInt(l - r)
        | MpDivide, AsFloats (l, r) -> MpFloat(l - r)
        | _ -> raise (NotSupportedException("Arithmetic operation is not supported"))

    and logical lhs op rhs =
        match op, lhs, rhs with
        | MpAnd, MpBool l, MpBool r -> MpBool(l && r)
        | MpOr, MpBool l, MpBool r -> MpBool(l || r)
        | MpXor, MpBool l, MpBool r -> MpBool((not l && r) || (not r && l))
        | _, _, _ -> raise (NotSupportedException("Logical operation is not supported"))

    and getIndices state (value: value) (indices: expr list) ind =
        let index = toInt (eval state indices[ind])

        match value with
        | MpArrayValue v ->
            if index < 0 || index >= v.Length then
                raise (IndexOutOfRangeException())

            if ind = indices.Length - 1 then
                v[index]
            else
                getIndices state v[index] indices (ind + 1)

        | _ -> raise (NotSupportedException())

    let rec setIndices state (value: value) (indices: expr list) ind (e: value) =
        let index = toInt (eval state indices[ind])

        match value with
        | MpArrayValue v ->
            if index < 0 || index >= v.Length then
                raise (IndexOutOfRangeException())

            if ind = indices.Length - 1 then
                v[index] <- e
            else
                setIndices state v[index] indices (ind + 1) e

        | _ -> raise (NotSupportedException())


    let mpRun (program: instruction[]) =
        let mutable pi = 0
        let variables = VarLookup()
        let forLoops = Dictionary<index, index * identifier * index * value>()
        let whileLoops = Dictionary<index, index>()
        let evalAux = eval variables

        let assign (value: assign) =
            match value with
            | Set (identifier, expr) -> variables[identifier] <- evalAux expr
            | SetE (identifier, expr) ->
                match identifier with
                | MpIndex (identifier, indices) ->
                    let value = variables[identifier]

                    setIndices variables value indices 0 (evalAux expr)

                | _ -> raise (NotSupportedException())

        let print (value: value) =
            match value with
            | MpInt x -> printfn $"%d{x}"
            | MpFloat x -> printfn $"%f{x}"
            | MpBool x -> printfn $"%b{x}"
            | MpString x -> printfn $"%s{x}"
            | MpNull -> printfn "null"
            | MpArrayValue x -> printfn $"%A{x}"

        let initBlock instruction =
            match instruction with
            | MpWhile _ -> true
            | MpFor _ -> true
            | MpIf _ -> true
            | MpElIf _ -> true
            | MpElse -> true
            | _ -> false

        let endBlock instruction =
            match instruction with
            | MpEnd -> true
            | _ -> false

        let rec findEndBlock ind cant =
            let mutable newCant = cant

            if initBlock program[ind] then
                newCant <- cant + 1

            if endBlock program[ind] then
                newCant <- cant - 1

            if newCant = 0 then ind else findEndBlock (ind + 1) newCant

        let rec findStartBlock ind cant =
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
            | MpPrint exp -> print (evalAux exp)
            | MpIf cond ->
                let condition = toBool (evalAux cond)

                if not condition then
                    let index = findEndBlock (pi + 1) 1
                    pi <- index

            | MpElse ->
                let indexStart = findStartBlock (pi - 1) 0

                match program[indexStart] with
                | MpIf cond ->
                    let condition = toBool (evalAux cond)

                    if condition then
                        let indexEnd = findEndBlock (pi + 1) 1
                        pi <- indexEnd
                | MpElIf cond ->
                    let condition = toBool (evalAux cond)

                    if condition then
                        let indexEnd = findEndBlock (pi + 1) 1
                        pi <- indexEnd
                | _ -> raise (NotSupportedException())

            | MpElIf condEl ->
                let indexStart = findStartBlock (pi - 1) 0

                match program[indexStart] with
                | MpIf condIf ->
                    let conditionIf = toBool (evalAux condIf)
                    let conditionEl = toBool (evalAux condEl)

                    if (conditionIf || not conditionEl) then
                        let indexEnd = findEndBlock (pi + 1) 1
                        pi <- indexEnd
                | MpElIf condIf ->
                    let conditionIf = toBool (evalAux condIf)
                    let conditionEl = toBool (evalAux condEl)

                    if (conditionIf || not conditionEl) then
                        let indexEnd = findEndBlock (pi + 1) 1
                        pi <- indexEnd
                | _ -> raise (NotSupportedException())

            | MpFor (identifier, init, stop, step) ->
                assign (Set(identifier, MpLiteral(MpInt init)))

                let index = findEndBlock (pi + 1) 1
                forLoops[index] <- (pi, identifier, stop, MpInt step)

                if toInt variables[identifier] >= stop then
                    pi <- index
            | MpWhile condition ->
                let index = findEndBlock (pi + 1) 1
                whileLoops[index] <- pi

                if evalAux condition |> toBool |> not then
                    pi <- index
            | MpEnd ->
                if whileLoops.ContainsKey(pi) then
                    pi <- whileLoops[pi] - 1

                if forLoops.ContainsKey(pi) then
                    let start, identifier, stop, step = forLoops[pi]
                    let x = variables[identifier]
                    variables[identifier] <- arithmetic x MpAdd step

                    if toInt variables[identifier] < stop then
                        pi <- start

            | MpExpr x ->
                let _ = evalAux x
                ()

        while pi < program.Length do
            step ()
            pi <- pi + 1
