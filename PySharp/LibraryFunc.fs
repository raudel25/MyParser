namespace MyParser

open System
open Microsoft.FSharp.Core
open FParsec

module internal LibraryFunc =
    let error (pos: Position) (s: string) =
        $"Error in Ln: {pos.Line} Col: {pos.Column}\n{s}"

    let rec toStr value =
        let aux (x: value[]) =
            let rec f i : string =
                if i = x.Length - 1 then
                    toStr x[x.Length - 1]
                else
                    $"{toStr x[i]} , {f (i + 1)}"

            if x.Length = 0 then "" else f 0

        match value with
        | MpInt x -> string x
        | MpDouble x -> string x
        | MpBool true -> "true"
        | MpBool false -> "false"
        | MpString x -> x
        | MpChar x -> string x
        | MpNull -> "null"
        | MpFuncStaticValue (x, y, _, _) ->
            if y.Length = 0 then
                $"{x} ()"
            else
                let y = List.map MpString y |> List.toArray
                $"{x} ( {aux y} )"

        | MpArrayValue x -> $"[ {aux x} ]"
        | MpTupleValue x -> $"( {aux x} )"
        | MpClassValue (x, y,_) ->
            if y.Count = 0 then
                x + "{}"
            else
                let y = List.map MpString (List.ofSeq y.Keys) |> List.toArray
                let s1 = x + " { "
                let s2 = (aux y) + " }"
                s1 + s2

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
