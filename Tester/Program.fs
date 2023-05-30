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

mpRun (mpParse "a=array(1,3); a[0]=[10,2]; b=a[0]; print(b[0]);")
