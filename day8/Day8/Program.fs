[<EntryPoint>]
let main argv =
    let filepath, part =
        match argv with
        | [| filepath; part |] -> (filepath, int part)
        | [| filepath |] -> (filepath, 1)
        | _ -> failwith "usage: dotnet run <input file> <part>"

    match part with
    | 1 -> TreetopTreeHouse.Part1.run filepath
    | 2 -> TreetopTreeHouse.Part2.run filepath
    | _ -> failwith "part in '1' or '2'"

    0
