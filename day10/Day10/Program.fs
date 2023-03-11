open CathodeRayTube

let loadInput inputpath =
    System.IO.File.ReadAllLines(inputpath) |> Seq.map Instruction.parse

let sumOfSignals instrs =
    VM.init
    |> VM.toSeq instrs
    |> Seq.filter (fun vm -> Array.contains vm.Cycle [| 20u; 60u; 100u; 140u; 180u; 220u |])
    |> Seq.map (fun vm -> int vm.Cycle * vm.X)
    |> Seq.sum

let SPRITE_WIDTH = 3
let CRT_ROW_WIDTH = 40
let generateRow = List.init CRT_ROW_WIDTH (fun i -> '.')


let (|StartOfRow|_|) input = if input = 1 then Some input else None


let (|Lit|Dark|) (x, spliteX) =
    if spliteX <= x && x <= spliteX + (SPRITE_WIDTH - 1) then
        Lit
    else
        Dark

let lit x (row: char list) =
    row[0 .. x - 1 - 1] @ [ '#' ] @ row[x..]

let renderCRT instrs =
    VM.init
    |> VM.toSeq instrs
    |> Seq.fold
        (fun crt vm ->
            let currentCrtX = (int vm.Cycle) % CRT_ROW_WIDTH

            let crt2 =
                match currentCrtX with
                | StartOfRow _ -> List.append crt [ generateRow ]
                | _ -> crt

            let currentRow = List.last crt2
            let spriteStartX = vm.X % CRT_ROW_WIDTH

            let nextCurrentRow =
                match (currentCrtX, spriteStartX) with
                | Lit -> lit currentCrtX currentRow
                | Dark -> currentRow

            crt2[.. crt2.Length - 2] @ [ nextCurrentRow ])
        []
    |> Seq.map System.String.Concat
    |> String.concat "\n"

let part1 inputPath =
    inputPath |> loadInput |> sumOfSignals |> printfn "%A"

let part2 inputPath =
    inputPath |> loadInput |> renderCRT |> printfn "%s"


[<EntryPoint>]
let main argv =
    let filepath, part =
        match argv with
        | [| filepath; part |] -> (filepath, int part)
        | [| filepath |] -> (filepath, 1)
        | _ -> failwith "usage: dotnet run <input file> <part>"

    match part with
    | 1 -> part1 filepath
    | 2 -> part2 filepath
    | _ -> failwith "part in '1' or '2'"

    0
