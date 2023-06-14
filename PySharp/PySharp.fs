namespace PySharp

open System.Collections.Generic
open Parser
open Interpreter
open LibraryFunc
open CircularReference
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

        let files = Dictionary<identifier, instruction[]>()

        let _ =
            mpRunAux (ProgramScope(files, (variables, functions, classes, Module modules), program))

        ()

    let mpModules =
        let m = Dictionary<identifier, instruction[]>()
        m

    let mpRun (program: instruction[]) (files: Dictionary<identifier, instruction[]>) =
        let variables = VarLookup()
        let functions = FunctionsLookup()
        let structs = ClassLookup()
        let modules = Module(Dictionary<identifier, Scope>())

        mpRunAux (ProgramScope(files, (variables, functions, structs, modules), program))

    let mpToStr value = toStr value

    let mpCircularReference dict = mpCircularReferenceInternal dict
