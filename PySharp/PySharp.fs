namespace PySharp

open PySharp.Parser
open PySharp.Interpreter
open PySharp.LibraryFunc
open FParsec

module PySharp =
    let mpParse (program: string) =
        match run mpLines program with
        | Success (result, _, _) -> result |> List.toArray
        | Failure (errorMsg, _, _) -> failwith errorMsg

    let mpState =
        let variables = VarLookup()
        let functions = FunctionsLookup()
        let structs = ClassLookup()

        State(variables, functions, structs)

    let mpInteractive (state: State) (program: instruction[]) =
        let variables, functions, structs = state

        let _ = mpRunAux (ProgramState(variables, functions, structs, program))
        ()

    let mpRun (program: instruction[]) =
        let variables = VarLookup()
        let functions = FunctionsLookup()
        let structs = ClassLookup()

        mpRunAux (ProgramState(variables, functions, structs, program))

    let mpToStr value = toStr value
