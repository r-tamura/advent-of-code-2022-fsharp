
open DistressSignal

let part1 inputFilePath =
    let input = System.IO.File.ReadAllText inputFilePath
    let packets = Parser.parse input

    packets
    |> List.map Packet.comparePair
    |> List.mapi (fun i result -> (i + 1, result))
    |> List.filter (fun (i, result) -> result = Packet.RightOrder)
    |> List.map (fun (i, _) -> i)
    |> List.sum
    |> printfn "%A"


let part2 inputFilePath  =
    let DIVIDERS = [List [Number 2]; List[Number 6]]
    let input = System.IO.File.ReadAllText inputFilePath
    let packets = (Parser.parseAsList input) @ DIVIDERS

    packets
    |> List.sortWith (
        fun p1 p2  ->
            match Packet.comparePair (p1, p2) with
            | Packet.RightOrder -> -1
            | Packet.WrongOrder -> 1
            | Packet.Same -> 0
    )
    |> List.mapi (fun i p -> (i+1, p))
    |> List.filter (fun (i, p) -> (List.contains p DIVIDERS))
    |> List.map (fun (i, _) -> i)
    |> List.take 2
    |> List.reduce (*)
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