open RegolithReservoir

let part1 (inputFilePath: string) =
    let rockPaths = inputFilePath |> System.IO.File.ReadAllText |> Parser.parse
    let cave = Cave.create Cave.EndlessVoid rockPaths
    let cave', sandPositions = Cave.dropUnitsOfSandUntilFlowingIntoVoid cave
    printfn "%A" sandPositions.Length

let part2 (inputFilePath: string) =
    inputFilePath
    |> System.IO.File.ReadAllText
    |> Parser.parse
    |> Cave.create (Cave.InfiniteHorizontalLine 2)
    |> Cave.dropUnitsOfSandUntilAUnitOfSandComesToRestAtTheFlowingPoint
    |> Cave.countSand
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
