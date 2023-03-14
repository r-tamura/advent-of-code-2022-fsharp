open MonkeyInTheMiddle

let rounds count initialMonkeys =
    seq { 0 .. count - 1 }
    |> Seq.fold (fun monkeys _ -> MonkeyCollection.round monkeys) initialMonkeys

let part1 inputFilePath =
    inputFilePath
    |> Parser.read
    |> Seq.toList
    |> rounds 20
    |> List.map (fun monkey -> monkey.Stats)
    |> List.sortDescending
    |> List.take 2
    |> List.reduce (fun acc count -> acc * count)
    |> printfn "%A"

let part2 inputFilePath =
    inputFilePath |> (fun _ -> failwith "part 2 solution is not implemented")

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
