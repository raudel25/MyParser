namespace MyParser

open System
open Microsoft.FSharp.Core

module internal LibraryFunc =
    let rec toStr value =
        match value with
        | MpInt x -> string x
        | MpFloat x -> string x
        | MpBool true -> "true"
        | MpBool false -> "false"
        | MpString x -> x
        | MpNull -> "null"
        | MpArrayValue x ->
            let mutable s = "[ "

            for i in 0 .. x.Length - 2 do
                s <- s + toStr x[i]
                s <- s + " , "

            s <- s + toStr x[x.Length - 1]
            s <- s + " ]"

            s

    let printL (value: value) : value =
        match value with
        | MpInt x -> printf $"%d{x}"
        | MpFloat x -> printf $"%f{x}"
        | MpBool x -> printf $"%b{x}"
        | MpString x -> printf $"%s{x}"
        | MpNull -> printf "null"
        | MpArrayValue _ -> printf $"%s{(toStr value)}"

        MpNull

    let printLn (value: value) =
        let _ = printL value
        printf "\n"

        MpNull

    let input =
        let s = Console.ReadLine()

        MpString s

    let toInt value =
        match value with
        | MpInt x -> MpInt x
        | MpFloat x -> MpInt(int x)
        | MpBool true -> MpInt 1
        | MpBool false -> MpInt 0
        | MpString x -> MpInt(int x)
        | MpNull -> MpInt 0
        | MpArrayValue _ -> raise (NotSupportedException("Cannot convert from array to int"))

    let toFloat value =
        match value with
        | MpInt x -> MpFloat(float x)
        | MpFloat x -> MpFloat x
        | MpBool true -> MpFloat 1
        | MpBool false -> MpFloat 0
        | MpString x -> MpFloat(float x)
        | MpNull -> MpFloat 0
        | MpArrayValue _ -> raise (NotSupportedException("Cannot convert from array to int"))

    let funcLib0 s =
        match s with
        | "input" -> input
        | _ -> MpNull

    let funcLib1 s value =
        match s with
        | "printL" -> printL value
        | "printLn" -> printLn value
        | "int" -> toInt value
        | "float" -> toFloat value
        | "str" -> MpString(toStr value)
        | _ -> MpNull
