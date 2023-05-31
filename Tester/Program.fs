open MyParser.Parser
open MyParser.Interpreter
open FParsec

let runParser (parser: Parser<_, unit>) (input: string) =
    match run parser input with
    | Success (result, _, p) ->
        printf "%A" p
        Some result
    | Failure (_, _, _) -> None

let input = "[1,4,true ]"
let result = runParser mpArray input

match result with
| Some x -> printfn "Parsed %A" x
| None -> printfn "Failed to parse"

mpRun (mpParse "for i in 10,20 {print(i);}")
