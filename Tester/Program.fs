open MyParser.Parser
open MyParser.Interpreter
open FParsec

let runParser (parser: Parser<_, unit>) (input: string) =
    match run parser input with
    | Success (result, _, p) ->
        printf "%A" p
        Some result
    | Failure (_, _, _) -> None

let input = "while ( 2>2 ) \n{"
let result = runParser mpWhile input

match result with
| Some x -> printfn "Parsed %A" x
| None -> printfn "Failed to parse"

mpRun (mpParse "a=0; while(a<10){ print(a); a=a+1; while(false){}}")
