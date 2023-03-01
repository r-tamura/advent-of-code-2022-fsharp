[<EntryPoint>]
let main argv =
    let filepath, part =
        match argv with
        | [| filepath; part |] -> (filepath, int part)
        | [| filepath |] -> (filepath, 1)
        | _ -> failwith "usage: dotnet run <input file> <part>"


    TreetopTreeHouse.Part1.run filepath

    0
