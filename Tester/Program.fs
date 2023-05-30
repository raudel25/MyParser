open MyParser.Parser
open FParsec

let runParser (parser: Parser<_, unit>) (input: string) =
    match run parser input with
    | Success (result, _, _) -> Some result
    | Failure (_, _, _) -> None

let input = "a=a==2"
let result = runParser mpAssign input

match result with
| Some x -> printfn "Parsed float"
| None -> printfn "Failed to parse float"
