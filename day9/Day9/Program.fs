open RopeBridge

let loadInput inputpath =
    System.IO.File.ReadAllLines(inputpath) |> Array.map Motion.parse

let part1 inputpath =
    let initialState = State.init

    inputpath
    |> loadInput
    |> Array.fold (fun acc motion -> State.step motion acc) initialState
    |> State.countVisited
    |> printfn "%A"

[<EntryPoint>]
let main argv =
    let filepath, part =
        match argv with
        | [| filepath; part |] -> (filepath, int part)
        | [| filepath |] -> (filepath, 1)
        | _ -> failwith "usage: dotnet run <input file> <part>"

    match part with
    | 1 -> part1 filepath
    // | 2 -> part2 filepath
    | _ -> failwith "part in '1' or '2'"

    0
