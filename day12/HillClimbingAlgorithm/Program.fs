open HillClimbing

let part1 inputFilePath =
    inputFilePath
    |> read
    |> Array.toList
    |> Parser.parse
    |> fewestStepCount
    |> printfn "%A"

let part2 inputFilePath =
    inputFilePath
    |> read
    |> Array.toList
    |> Parser.parse
    |> fewestStepCounFromEnd
    |> printfn "%A"

[<EntryPoint>]
let main argv =
    let inputFilePath, part =
        match argv with
        | [| inputFilePath; part |] -> (inputFilePath, int part)
        | [| inputFilePath |] -> (inputFilePath, 1)
        | _ -> failwith "usage: dotnet run <input file> <part>"

    match part with
    | 1 -> part1 inputFilePath
    | 2 -> part2 inputFilePath
    | _ -> failwith "part in '1' or '2'"

    0
