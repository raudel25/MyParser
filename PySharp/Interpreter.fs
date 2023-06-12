namespace PySharp

open System
open System.Collections.Generic
open FParsec
open Microsoft.FSharp.Core
open PySharp.LibraryFunc

module internal Interpreter =

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

    let globalFunVars (variables: VarLookup) vars =
        let contains var =
            List.exists (fun (i, _) -> i = var) vars

        let rec f l =
            match l with
            | l :: t -> if not (contains l) then l :: (f t) else (f t)
            | [] -> []

        let variables = List.ofSeq variables.Keys

        (f variables)

    let checkFuncVars vars (functions: FunctionsLookup) (classes: ClassLookup) =
        let f (i, p) =
            if functions.ContainsKey(i) || classes.ContainsKey(i) then
                raise (Exception(error p "There are two terms with the same name"))

        let _ = List.map f vars
        ()

    type stateFunction =
        | Execute
        | Break of uint8 * Position
        | Continue of uint8 * Position
        | Return of value

    let rec eval (state: ProgramState) (expr: expr) =
        let (variables: VarLookup, functions: FunctionsLookup, classes: ClassLookup, _: instruction[]) =
            state

        let expr, pos = expr

        match expr with
        | MpLiteral x -> x

        | MpVar identifier ->
            if functions.ContainsKey(identifier) then
                match functions[identifier] with
                | Static (l, g, b) -> MpFuncStaticValue(identifier, l, g, b, (variables, functions))
                | _ -> raise (Exception(error pos "Cannot access an instance function in a static context"))
            elif classes.ContainsKey(identifier) then
                let l, f = classes[identifier]
                MpClassValue(identifier, l, f)
            elif variables.ContainsKey(identifier) then
                variables[identifier]
            else
                raise (Exception(error pos "Variable does not exist"))

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
            if classes.ContainsKey(identifier) then
                let l, f = classes[identifier]
                let value = MpClassValue(identifier, l, f)

                getProp state value indices 0
            elif variables.ContainsKey(identifier) then
                let value = variables[identifier]

                getProp state value indices 0
            else
                raise (Exception(error pos "Variable does not exist"))

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

            let functionParams
                (identifiers: identifier list)
                (globals: identifier list)
                (variables: VarLookup)
                (functions: FunctionsLookup)
                =
                let vars = VarLookup()
                let newFunctions = FunctionsLookup()
                let newClasses = ClassLookup()

                if identifiers.Length <> expr.Length then
                    raise (Exception(error pos "Function does not have correct parameters"))

                let _ =
                    List.map (fun i -> vars[identifiers[i]] <- (eval state expr[i])) [ 0 .. identifiers.Length - 1 ]

                let _ = List.map (fun var -> vars[var] <- variables[var]) globals

                let _ =
                    List.map (fun f -> newFunctions[f] <- functions[f]) (List.ofSeq functions.Keys)

                let _ = List.map (fun f -> newClasses[f] <- classes[f]) (List.ofSeq classes.Keys)

                (vars, newFunctions, newClasses)

            match s with
            | MpFuncStaticValue (_, identifiers, globals, block, (v, f)) ->
                let vars, newFunctions, newClasses = functionParams identifiers globals v f

                let aux = mpRunAux (ProgramState(vars, newFunctions, newClasses, block))

                let _ = List.map (fun var -> variables[var] <- vars[var]) globals

                aux
            | MpFuncSelfValue (_, identifiers, globals, block, (v, f), value) ->
                let vars, newFunctions, newClasses = functionParams identifiers globals v f
                vars["self"] <- value

                let aux = mpRunAux (ProgramState(vars, newFunctions, newClasses, block))

                let _ = List.map (fun var -> variables[var] <- vars[var]) globals

                aux
            | _ -> raise (Exception(error pos "Expression is not function"))

        | MpReservedFunc0 s -> funcLib0 s

        | MpReservedFunc1 (s, expr) ->
            let _, pos = expr
            funcLib1 pos s (eval state expr)

        | MpTernary (cond, e1, e2) ->
            let cond = eval state cond

            if (toBool pos cond) then eval state e1 else eval state e2

        | MpClassConst (expr, props) ->
            let _, pos = expr
            let value = eval state expr

            match value with
            | MpClassValue (s, listProps, dictFunc) ->
                if listProps.Length <> props.Length then
                    raise (Exception(error pos "Class does not have correct parameters"))

                let q = Dictionary<identifier, value>()

                let _ =
                    List.map (fun i -> q[listProps[i]] <- (eval state props[i])) [ 0 .. listProps.Length - 1 ]

                MpObjectValue(s, q, dictFunc)
            | _ -> raise (Exception(error pos "Expression is not class"))

        | MpLambda (vars, block) ->
            let newVars = List.map fst vars
            checkFuncVars vars functions classes
            let globals = globalFunVars variables vars
            MpFuncStaticValue("lambda", newVars, globals, block, (variables, functions))

        | MpSelf ->
            if not (variables.ContainsKey("self")) then
                raise (Exception(error pos "Incorrect instruction self"))

            variables["self"]

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
            | MpObjectValue (_, props, (v, f)) ->
                if f.ContainsKey(prop) && ind = indices.Length - 1 then
                    match f[prop] with
                    | Static (vars, g, b) ->
                        let value = MpFuncStaticValue(prop, vars, g, b, (v, f))
                        getValue value
                    | Self (vars, g, b) ->
                        let value = MpFuncSelfValue(prop, vars, g, b, (v, f), value)
                        getValue value
                elif props.ContainsKey(prop) then
                    getValue props[prop]
                elif v.ContainsKey(prop) then
                    getValue v[prop]
                else
                    raise (Exception(error pos "The property is not correct"))
            | MpClassValue (_, _, (v, f)) ->
                if f.ContainsKey(prop) then
                    match f[prop] with
                    | Static (vars, g, b) ->
                        let value = MpFuncStaticValue(prop, vars, g, b, (v, f))
                        getValue value
                    | _ -> raise (Exception(error pos "The property is not correct"))
                elif v.ContainsKey(prop) then
                    getValue v[prop]
                else
                    raise (Exception(error pos "The property is not correct"))
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
            | MpObjectValue (_, props, (v, _)) ->
                if props.ContainsKey(prop) then
                    if ind = indices.Length - 1 then
                        props[prop] <- e
                    else
                        setProp state props[prop] indices (ind + 1) e
                elif v.ContainsKey(prop) then
                    if ind = indices.Length - 1 then
                        v[prop] <- e
                    else
                        setProp state v[prop] indices (ind + 1) e
                else
                    raise (Exception(error pos "The property is not correct"))
            | _ -> raise (Exception(error pos "The property is not correct"))

    and mpRunAux (state: ProgramState) =
        let (variables: VarLookup, functions: FunctionsLookup, classes: ClassLookup, program: instruction[]) =
            state

        let evalAux = eval state

        let assign (value: assign) =
            match value with
            | Set (identifier, expr) ->
                let identifier, pos = identifier

                match identifier with
                | MpVar identifier ->
                    if functions.ContainsKey(identifier) || classes.ContainsKey(identifier) then
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
                || classes.ContainsKey(identifier)
            then
                raise (Exception(error pos "There are two terms with the same name"))

        let rec step instruction =
            match instruction with
            | MpAssign set ->
                assign set
                Execute

            | MpIf (cond, block) ->
                let _, pos = cond

                if toBool pos (evalAux cond) then
                    executeBlock block
                else
                    Execute

            | MpElse (cond, blockIf, blockElse) ->
                let _, pos = cond

                if toBool pos (evalAux cond) then
                    executeBlock blockIf
                else
                    executeBlock blockElse

            | MpElIf (condIf, blockIf, condElIf, blockElIf) ->
                let _, posIf = condIf
                let _, posElIf = condElIf

                if toBool posIf (evalAux condIf) then
                    executeBlock blockIf
                elif toBool posElIf (evalAux condElIf) then
                    executeBlock blockElIf
                else
                    Execute

            | MpElIfElse (condIf, blockIf, condElIf, blockElIf, blockElse) ->
                let _, posIf = condIf
                let _, posElIf = condElIf

                if toBool posIf (evalAux condIf) then
                    executeBlock blockIf
                elif toBool posElIf (evalAux condElIf) then
                    executeBlock blockElIf
                else
                    executeBlock blockElse

            | MpFor ((identifier, pos), initE, stopE, stepE, block) ->
                let toIntAux expr =
                    let value = (evalAux expr)
                    let _, pos = expr

                    match value with
                    | MpInt n -> n
                    | _ -> raise (Exception(error pos "Cannot convert to int"))

                let _, stop, step = (toIntAux initE, toIntAux stopE, toIntAux stepE)

                let _ = assign (Set((MpVar(identifier), pos), initE))

                let rec loop () =
                    if toInt pos variables[identifier] >= stop then
                        Execute
                    else

                        match executeBlock block with
                        | Execute ->
                            variables[identifier] <- arithmetic pos variables[identifier] MpAdd (MpInt step)
                            loop ()
                        | Return v -> Return v
                        | Break (index, pos) ->
                            if index = uint8 0 then
                                Execute
                            else
                                Break(index - (uint8 1), pos)
                        | Continue (index, pos) ->
                            if index = uint8 0 then
                                variables[identifier] <- arithmetic pos variables[identifier] MpAdd (MpInt step)
                                loop ()
                            else
                                Continue(index - (uint8 1), pos)

                loop ()

            | MpWhile (condition, block) ->
                let _, pos = condition

                let rec loop () =
                    if not (toBool pos (evalAux condition)) then
                        Execute
                    else
                        match executeBlock block with
                        | Execute -> loop ()
                        | Return v -> Return v
                        | Break (index, pos) ->
                            if index = uint8 0 then
                                Execute
                            else
                                Break(index - (uint8 1), pos)
                        | Continue (index, pos) ->
                            if index = uint8 0 then
                                loop ()
                            else
                                Continue(index - (uint8 1), pos)

                loop ()
                
            | MpLoop block ->
                let rec loop () =
                    match executeBlock block with
                    | Execute -> loop ()
                    | Return v -> Return v
                    | Break (index, pos) ->
                        if index = uint8 0 then
                            Execute
                        else
                            Break(index - (uint8 1), pos)
                    | Continue (index, pos) ->
                        if index = uint8 0 then
                            loop ()
                        else
                            Continue(index - (uint8 1), pos)

                loop ()

            | MpExpr x ->
                let _ = evalAux x
                Execute

            | MpFuncStatic ((identifier, pos), vars, block) ->
                let _ = sameName pos identifier

                let globals = globalFunVars variables vars
                let newVars = List.map fst vars
                functions[identifier] <- Static(newVars, globals, block)
                checkFuncVars vars functions classes

                Execute

            | MpFuncSelf ((identifier, pos), vars, block) ->
                let _ = sameName pos identifier

                let globals = globalFunVars variables vars
                let newVars = List.map fst vars
                functions[identifier] <- Self(newVars, globals, block)
                checkFuncVars vars functions classes

                Execute

            | MpReturn expr -> Return(evalAux expr)

            | MpBreak (index, pos) -> Break(index, pos)

            | MpContinue (index, pos) -> Continue(index, pos)

            | MpClass ((identifier, pos), vars) ->
                let _ = sameName pos identifier

                classes[identifier] <- vars, (VarLookup(), FunctionsLookup())

                Execute

            | MpComment -> Execute

            | MpImpl ((s, pos), block) ->
                if not (classes.ContainsKey(s)) then
                    raise (Exception(error pos "Class does not exist"))

                let _, (variables, functions) = classes[s]

                let newClasses = ClassLookup()

                let _ = List.map (fun x -> newClasses[x] <- classes[x]) (List.ofSeq classes.Keys)

                if functions.Count <> 0 then
                    raise (Exception(error pos "There are two implementations for the same class"))

                let _ = mpRunAux (variables, functions, classes, block)

                Execute

            | MpImplDeriving ((s1, p1), (s2, p2), block) ->
                if not (classes.ContainsKey(s1)) then
                    raise (Exception(error p1 "Class does not exist"))

                if not (classes.ContainsKey(s2)) then
                    raise (Exception(error p2 "Class does not exist"))

                let l1, (v1, functions1) = classes[s1]
                let l2, (v2, functions2) = classes[s2]

                if functions.Count <> 0 then
                    raise (Exception(error p1 "There are two implementations for the same class"))

                let checkProps x =
                    if List.contains x l1 then
                        raise (Exception(error p1 "There are two properties with the same name"))

                let _ = mpRunAux (v1, functions1, classes, block)


                let checkFunc x =
                    if not (functions1.ContainsKey(x)) then
                        functions1[x] <- functions2[x]

                let checkVar x =
                    if not (v1.ContainsKey(x)) then
                        v1[x] <- v2[x]


                let _ = List.map checkProps l2
                let _ = List.map checkFunc (List.ofSeq functions2.Keys)
                let _ = List.map checkVar (List.ofSeq v2.Keys)

                classes[s1] <- (List.append l1 l2, (v1, functions1))

                Execute

        and executeBlock block =
            let rec loop i =
                if i = block.Length then
                    Execute
                else
                    match step block[i] with
                    | Execute -> loop (i + 1)
                    | Return v -> Return v
                    | Break (index, pos) -> Break(index, pos)
                    | Continue (index, pos) -> Continue(index, pos)

            loop 0

        let rec loop i =
            if i = program.Length then
                MpNull
            else
                match step program[i] with
                | Execute -> loop (i + 1)
                | Return v -> v
                | Break (_, pos) -> raise (Exception(error pos "Incorrect instruction break"))
                | Continue (_, pos) -> raise (Exception(error pos "Incorrect instruction continue"))

        loop 0
