namespace PySharp

open System.Collections.Generic
open PySharp.Parser
open PySharp.Interpreter
open PySharp.LibraryFunc
open FParsec

module PySharp =
    let mpParse (program: string) =
        match run mpLines program with
        | Success (result, _, _) -> result |> List.toArray
        | Failure (errorMsg, _, _) -> failwith errorMsg

    let mpScope =
        let variables = VarLookup()
        let functions = FunctionsLookup()
        let structs = ClassLookup()
        let modules = Dictionary<identifier, Scope>()

        (variables, functions, structs, modules)

    let mpInteractive
        (variables: VarLookup)
        (functions: FunctionsLookup)
        (classes: ClassLookup)
        (modules: Dictionary<identifier, Scope>)
        (program: instruction[])
        =

        let _ =
            mpRunAux (ProgramScope((variables, functions, classes, Module modules), program))

        ()

    let mpRun (program: instruction[]) =
        let variables = VarLookup()
        let functions = FunctionsLookup()
        let structs = ClassLookup()
        let modules = Module(Dictionary<identifier, Scope>())

        mpRunAux (ProgramScope((variables, functions, structs, modules), program))

    let mpToStr value = toStr value
