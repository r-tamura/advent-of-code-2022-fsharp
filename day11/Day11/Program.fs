open MonkeyInTheMiddle

let rounds count adjuster initialMonkeys =
    seq { 0 .. count - 1 }
    |> Seq.fold (fun monkeys _ -> MonkeyCollection.round adjuster monkeys) initialMonkeys

let part1 inputFilePath =
    let bored level = (/) level 3UL

    inputFilePath
    |> Parser.read
    |> Seq.toList
    |> rounds 20 bored
    |> List.map (fun monkey -> monkey.Stats)
    |> List.sortDescending
    |> List.take 2
    |> List.reduce (fun acc count -> acc * count)
    |> printfn "%A"

let part2 inputFilePath =
    let monkeys = inputFilePath |> Parser.read |> Seq.toList

    let commonMultiple =
        monkeys
        |> List.map ((fun m -> m.Pick) >> (fun (divisible, t, f) -> divisible))
        |> List.reduce (fun acc divisible -> acc * divisible)

    let easy level = (%) level commonMultiple

    monkeys
    |> rounds 10000 easy
    |> List.map (fun monkey -> monkey.Stats)
    |> List.sortDescending
    |> List.take 2
    |> List.reduce (fun acc count -> acc * count)
    |> printfn "%d"

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
