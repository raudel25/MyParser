﻿open MyParser.Parser
open MyParser.Interpreter
open FParsec

let runParser (parser: Parser<_, unit>) (input: string) =
    match run parser input with
    | Success (result, _, p) ->
        printf "%A" p
        Some result
    | Failure (_, _, _) -> None

let input = "a()"
let result = runParser mpInvoke input

match result with
| Some x -> printfn "Parsed %A" x
| None -> printfn "Failed to parse"

let a = mpRun (mpParse "a=[1+2;2-3]; printLn(a);")
