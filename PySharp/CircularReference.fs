namespace PySharp

open System
open System.Collections.Generic
open LibraryFunc

module internal CircularReference =
    let rec mpStepAdj (m: string) (block: instruction[]) (adj: List<int>[]) (dict: Dictionary<string, int>) =
        let index = dict[m]

        let exeB b = mpStepAdj m b adj dict

        let step (instr: instruction) =
            match instr with
            | MpIf (_, b) -> exeB b
            | MpElse (_, b1, b2) ->
                let _ = exeB b1
                exeB b2
            | MpElIf (_, b1, _, b2) ->
                let _ = exeB b1
                exeB b2
            | MpElIfElse (_, b1, _, b2, b3) ->
                let _ = exeB b1
                let _ = exeB b2
                exeB b3
            | MpLoop b -> exeB b
            | MpWhile (_, b) -> exeB b
            | MpFor (_, _, _, _, b) -> exeB b
            | MpFuncSelf (_, _, b) -> exeB b
            | MpFuncStatic (_, _, b) -> exeB b
            | MpImpl (_, b) -> exeB b
            | MpImplDeriving (_, _, b) -> exeB b
            | MpModule (_, b) -> exeB b
            | MpImport (s, pos) ->
                if s = "main" then
                    raise (Exception(error pos $"The file {s}.ps cannot be imported"))

                if not (dict.ContainsKey(s)) then
                    raise (Exception(error pos $"The file {s}.ps does not exist"))

                adj[ index ].Add(dict[s])
            | _ -> ()

        let _ = Array.map step block
        ()

    let mpBuildAdj (modules: Dictionary<string, instruction[]>) =
        let dict = Dictionary<string, int>()
        let l = List.ofSeq modules.Keys

        let _ = List.map (fun i -> dict[l[i]] <- i) [ 0 .. l.Length - 1 ]

        let adj = Array.create l.Length null
        let _ = List.map (fun i -> adj[i] <- List<int>()) [ 0 .. l.Length - 1 ]

        let _ = List.map (fun s -> mpStepAdj s modules[s] adj dict) l

        adj

    let mpCircularReferenceInternal (modules: Dictionary<string, instruction[]>) =
        let adj = mpBuildAdj modules

        let inDegree = Array.create adj.Length 0

        let aux i =
            let _ = List.map (fun j -> inDegree[j] <- inDegree[j] + 1) (List.ofSeq adj[i])
            ()

        let _ = List.map aux [ 0 .. adj.Length - 1 ]

        let q = Queue<int>()

        let _ =
            List.map
                (fun i ->
                    if inDegree[i] = 0 then
                        q.Enqueue(i))
                [ 0 .. adj.Length - 1 ]

        let aux i =
            inDegree[i] <- inDegree[i] - 1

            if inDegree[i] = 0 then q.Enqueue(i) else ()

        let rec f cant =
            if q.Count = 0 then
                cant
            else
                let n = q.Peek()
                let _ = q.Dequeue()

                let _ = List.map aux (List.ofSeq adj[n])
                f cant + 1

        f 0 <> adj.Length
