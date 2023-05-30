open MyParser.Parser
open FParsec

let runParser (parser: Parser<string, unit>) (input: string) =
    match run parser input with
    | Success(result, _, _) -> Some result
    | Failure(_, _, _) -> None

let input = "aaa2"
let result = runParser mpIdentifier input

match result with
| Some x -> printfn "Parsed float: %s" x
| None -> printfn "Failed to parse float"