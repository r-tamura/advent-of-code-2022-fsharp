open CathodeRayTube

let loadInput inputpath =
    System.IO.File.ReadAllLines(inputpath) |> Seq.map Instruction.parse


let action vm =
    printfn "cycle = %d, x = %d" vm.Cycle vm.X

let run instrs =
    VM.init
    |> VM.toSeq instrs
    |> Seq.filter (fun vm -> Array.contains vm.Cycle [| 20u; 60u; 100u; 140u; 180u; 220u |])
    |> Seq.map (fun vm -> int vm.Cycle * vm.X)
    |> Seq.sum

let part1 inputPath =
    inputPath |> loadInput |> run |> printfn "%A"

[<EntryPoint>]
let main argv =
    let filepath, part =
        match argv with
        | [| filepath; part |] -> (filepath, int part)
        | [| filepath |] -> (filepath, 1)
        | _ -> failwith "usage: dotnet run <input file> <part>"

    match part with
    | 1 -> part1 filepath
    | 2 -> failwith "not implemented"
    | _ -> failwith "part in '1' or '2'"

    0
