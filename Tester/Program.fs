open MyParser.Parser
open MyParser.Interpreter
open FParsec

let runParser (parser: Parser<_, unit>) (input: string) =
    match run parser input with
    | Success (result, _, p) ->
        printf "%A" p
        Some result
    | Failure (_, _, _) -> None

let input = "for a in 2,3,4 {"
let result = runParser mpFor input

match result with
| Some x -> printfn "Parsed %A" x
| None -> printfn "Failed to parse"

mpRun (mpParse "for a in 0,2,1 { for b in 0,2,1 {print(1);}}")
