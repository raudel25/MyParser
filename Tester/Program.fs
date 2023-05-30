open MyParser.Parser
open MyParser.Interpreter
open FParsec

let runParser (parser: Parser<_, unit>) (input: string) =
    match run parser input with
    | Success (result, _, p) ->
        printf "%A" p
        Some result
    | Failure (_, _, _) -> None

let input = "for =2"
let result = runParser mpAssign input

match result with
| Some x -> printfn "Parsed %A" x
| None -> printfn "Failed to parse"

mpRun (mpParse "a=2;print(2 +a+3); a=0; q=12;")
