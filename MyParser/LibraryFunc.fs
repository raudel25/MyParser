namespace MyParser

open System

module internal LibraryFunc =
    let rec printL (value: value) : value =
        match value with
        | MpInt x -> printf $"%d{x}"
        | MpFloat x -> printf $"%f{x}"
        | MpBool x -> printf $"%b{x}"
        | MpString x -> printf $"%s{x}"
        | MpNull -> printf "null"
        | MpArrayValue x ->
            printf "[ "

            for i in 0 .. x.Length - 2 do
                let _ = printL x[i]
                printf " , "

            let _ = printL x[x.Length - 1]
            printf " ]"

        MpNull

    let printLn (value: value) =
        let _ = printL value
        printf "\n"

        MpNull

    let input =
        let s = Console.ReadLine()

        MpString s

    let funcLib0 s =
        match s with
        | "input" -> input
        | _ -> MpNull

    let funcLib1 s value =
        match s with
        | "printL" -> printL value
        | "printLn" -> printLn value
        | _ -> MpNull
