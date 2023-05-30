namespace MyParser

open System
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

    /// Converts value to obj
    let toObj =
        function
        | MpBool x -> box x
        | MpInt x -> box x
        | MpFloat x -> box x
        | MpString x -> box x
        | MpNull -> null
    // | MpArray x -> raise (System.NotSupportedException(x.ToString()))

    /// Converts value to int
    let toInt =
        let f x =
            match x with
            | MpInt x -> x
            | MpFloat x -> int x
            | MpString x -> int x
            | MpNull -> 0
            | _ -> raise (NotSupportedException())

        f

    let toBool =
        let f x =
            match x with
            | MpBool x -> x
            | _ -> raise (NotSupportedException())

        f

    let toArray =
        function
        | MpArray x -> x
        | _ -> raise (NotSupportedException())

    /// Coerces a tuple of numeric values to double
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
        | _ -> raise (System.NotSupportedException $"%A{lhs} %A{rhs}")

    open System.Collections.Generic

    type VarLookup = Dictionary<identifier, value>

    /// Evaluates expressions
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
        | MpIndex (identifier, ind) ->
            let value = vars[identifier]
            let index = toInt (eval state ind)

            match value with
            | MpArrayValue v ->
                if index < 0 || index >= v.Length then
                    raise (System.IndexOutOfRangeException())

                v[index]
            | _ -> raise (NotSupportedException())
        // | MpGetAt (Location (identifier, [ index ])) ->
        //     let array = vars.[identifier] |> toArray
        //     array.[eval state index]
        // | MpGetAt (Location (identifier, xs)) -> raise (System.NotSupportedException())
        // | MpFunc (call) -> invoke state call
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
        | _ -> raise (System.NotSupportedException())

    and logical lhs op rhs =
        match op, lhs, rhs with
        | MpAnd, MpBool l, MpBool r -> MpBool(l && r)
        | MpOr, MpBool l, MpBool r -> MpBool(l || r)
        | MpXor, MpBool l, MpBool r -> MpBool((not l && r) || (not r && l))
        | _, _, _ -> raise (System.NotSupportedException())

    // and invoke state invoke =
    //     match invoke with
    //     | Method (tn, name, args) ->
    //         let t = getLibraryType tn
    //         let mi = t.GetMethod(name)
    //         let args = args |> Array.map (eval state >> toObj)
    //         mi.Invoke(null, args) |> fromObj
    //     | PropertyGet (tn, name) ->
    //         let t = getLibraryType tn
    //         let pi = t.GetProperty(name)
    //         pi.GetValue(null) |> fromObj

    /// Runs program
    let mpRun (program: instruction[]) =
        /// Program index
        let mutable pi = 0
        /// Variable lookup
        let variables = VarLookup()
        /// For from EndFor lookup
        let forLoops = Dictionary<index, index * identifier * index * value>()
        /// While from EndWhile lookup
        let whileLoops = Dictionary<index, index>()
        /// Call stack for Gosubs
        let callStack = Stack<index>()
        /// Evaluates expression with variables
        let evalAux = eval variables

        /// Assigns variable with result of expression
        let assign (value: assign) =
            match value with
            | Set (identifier, expr) -> variables[identifier] <- evalAux expr
            | SetE (identifier, expr) ->
                match identifier with
                | MpIndex (identifier, ind) ->
                    let value = variables[identifier]
                    let index = toInt (eval variables ind)

                    match value with
                    | MpArrayValue v ->
                        if index < 0 || index >= v.Length then
                            raise (IndexOutOfRangeException())

                        v[index] <- (eval variables expr)
                    | _ -> raise (NotSupportedException())
                | _ -> raise (NotSupportedException())


        let print (value: value) =
            match value with
            | MpInt x -> printfn $"%d{x}"
            | MpFloat x -> printfn $"%f{x}"
            | MpBool x -> printfn $"%b{x}"
            | MpString x -> printfn $"%s{x}"
            | MpNull -> printfn "null"

        /// Sets property with result of expression
        // let propertySet (tn, pn, expr) =
        //     let t = getLibraryType tn
        //     let pi = t.GetProperty(pn)
        //     pi.SetValue(null, eval expr |> toObj)
        //
        // /// Obtains an array for the specified identifier
        // let obtainArray identifier =
        //     match variables.TryGetValue(identifier) with
        //     | true, Array (array) -> array
        //     | true, _ -> raise (System.NotSupportedException())
        //     | false, _ ->
        //         let array = Hashtable<value, value>()
        //         variables.Add(identifier, Array(array))
        //         array

        // /// Sets array value at index with result of expression
        // let setAt (identifier, index, expr) =
        //     let array = obtainArray identifier
        //     array.[eval index] <- eval expr

        /// Finds first index of instructions
        ///
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


        // let isIf =
        //     function
        //     | If (_) -> true
        //     | _ -> false
        //
        // let isElseIf =
        //     function
        //     | ElseIf (_) -> true
        //     | _ -> false
        //
        // let isElse = (=) Else
        // let isEndIf = (=) EndIf
        //
        // let isFor =
        //     function
        //     | For (_, _, _) -> true
        //     | _ -> false
        //
        // let isEndFor = (=) EndFor
        //
        // let isWhile =
        //     function
        //     | While (_) -> true
        //     | _ -> false
        //
        // let isEndWhile = (=) EndWhile
        // let isFalse _ = false

        /// Instruction step
        let step () =
            let instruction = program[pi]

            match instruction with
            | MpAssign set -> assign set
            | MpPrint exp -> print (evalAux exp)
            // | PropertySet (tn, pn, expr) -> propertySet (tn, pn, expr)
            // | SetAt (Location (identifier, [ index ]), expr) -> setAt (identifier, index, expr)
            // | SetAt (_) -> raise (System.NotImplementedException())
            // | Action (call) -> invoke variables call |> ignore
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
                | _ -> raise (System.NotSupportedException())

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
                | _ -> raise (System.NotSupportedException())



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

        // | Sub (identifier) -> pi := findIndex (!pi + 1) (isFalse, isFalse) EndSub
        // | GoSub (identifier) ->
        //     let index = findIndex 0 (isFalse, isFalse) (Sub(identifier))
        //     callStack.Push(!pi)
        //     pi := index
        // | EndSub -> pi := callStack.Pop()
        // | Label (label) -> ()
        // | Goto (label) -> pi := findIndex 0 (isFalse, isFalse) (Label(label))

        while pi < program.Length do
            step ()
            pi <- pi + 1
