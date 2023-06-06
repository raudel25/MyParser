namespace MyParser

open System
open Microsoft.FSharp.Core
open FParsec

module LibraryFunc =
    let error (pos: Position) (s: string) =
        $"Error in Ln: {pos.Line} Col: {pos.Column}\n{s}"

    let rec toStr value =
        let aux (x: value[]) =
            if x.Length = 0 then
                ""
            else
                let mutable s = ""

                for i in 0 .. x.Length - 2 do
                    s <- s + toStr x[i]
                    s <- s + " , "

                s <- s + toStr x[x.Length - 1]

                s

        match value with
        | MpInt x -> string x
        | MpDouble x -> string x
        | MpBool true -> "true"
        | MpBool false -> "false"
        | MpString x -> x
        | MpChar x -> string x
        | MpNull -> "null"
        | MpFuncValue (x, y, _, _) ->
            if y.Length = 0 then
                x + " ()"
            else
                let mutable s = x + " ( "

                for i in 0 .. y.Length - 2 do
                    s <- s + y[i]
                    s <- s + " , "

                s <- s + y[y.Length - 1] + " )"

                s

        | MpArrayValue x ->
            let mutable s = "[ "
            s <- s + (aux x)
            s <- s + " ]"
            s
        | MpTupleValue x ->
            let mutable s = "( "
            s <- s + (aux x)
            s <- s + " )"
            s
        | MpStructValue (x, y) ->
            let mutable s = x + " { "

            for i in y do
                s <- s + $"{i.Key} = {toStr i.Value} , "

            s <- s[.. s.Length - 3] + "}"
            s

    let toChar pos value =
        match value with
        | MpInt n -> MpChar(char n)
        | MpChar c -> MpChar c
        | _ -> raise (Exception(error pos "Cannot convert to char"))

    let printL (value: value) : value =
        match value with
        | MpInt x -> printf $"%d{x}"
        | MpDouble x -> printf $"%f{x}"
        | MpBool x -> printf $"%b{x}"
        | MpString x -> printf $"%s{x}"
        | MpChar x -> printf $"%c{x}"
        | MpNull -> printf "null"
        | _ -> printf $"%s{toStr value}"

        MpNull

    let printLn (value: value) =
        let _ = printL value
        printf "\n"

        MpNull

    let input =
        let s = Console.ReadLine()

        MpString s

    let toInt pos value =
        try
            match value with
            | MpInt x -> MpInt x
            | MpDouble x -> MpInt(int x)
            | MpBool true -> MpInt 1
            | MpBool false -> MpInt 0
            | MpString x -> MpInt(int x)
            | MpChar x -> MpInt(int x)
            | MpNull -> MpInt 0
            | _ -> raise (Exception())

        with _ ->
            raise (Exception(error pos "Cannot convert from array to int"))

    let toDouble pos value =
        try
            match value with
            | MpInt x -> MpDouble(double x)
            | MpDouble x -> MpDouble x
            | MpBool true -> MpDouble 1
            | MpBool false -> MpDouble 0
            | MpString x -> MpDouble(double x)
            | MpChar x -> MpDouble(double x)
            | MpNull -> MpDouble 0
            | _ -> raise (Exception())
        with _ ->
            raise (Exception(error pos "Cannot convert to double"))


    let funcLib0 s =
        match s with
        | "input" -> input
        | _ -> MpNull

    let funcLib1 pos s value =
        match s with
        | "printL" -> printL value
        | "printLn" -> printLn value
        | "int" -> toInt pos value
        | "double" -> toDouble pos value
        | "str" -> MpString(toStr value)
        | "char" -> toChar pos value
        | _ -> MpNull
