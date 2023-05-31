namespace MyParser

open System
open Microsoft.FSharp.Core

module internal LibraryFunc =
    let rec toStr value =
        match value with
        | MpInt x -> string x
        | MpDouble x -> string x
        | MpBool true -> "true"
        | MpBool false -> "false"
        | MpString x -> x
        | MpChar x -> string x
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
        | MpDouble x -> printf $"%f{x}"
        | MpBool x -> printf $"%b{x}"
        | MpString x -> printf $"%s{x}"
        | MpChar x -> printf $"%c{x}"
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
        | MpDouble x -> MpInt(int x)
        | MpBool true -> MpInt 1
        | MpBool false -> MpInt 0
        | MpString x -> MpInt(int x)
        | MpChar x -> MpInt(int x)
        | MpNull -> MpInt 0
        | MpArrayValue _ -> raise (NotSupportedException("Cannot convert from array to int"))

    let toDouble value =
        match value with
        | MpInt x -> MpDouble(double x)
        | MpDouble x -> MpDouble x
        | MpBool true -> MpDouble 1
        | MpBool false -> MpDouble 0
        | MpString x -> MpDouble(double x)
        | MpChar x -> MpDouble(double x)
        | MpNull -> MpDouble 0
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
        | "double" -> toDouble value
        | "str" -> MpString(toStr value)
        | _ -> MpNull
