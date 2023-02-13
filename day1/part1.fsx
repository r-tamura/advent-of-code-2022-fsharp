let args = fsi.CommandLineArgs |> Array.tail

if args.Length < 1 then
    failwith "You need to specify input data file path"

let inputFilePath = args[0]

let lines: string[] = System.IO.File.ReadAllLines(inputFilePath)


let tryParseInt (str: string) : Option<int> =
    try
        str |> int |> Some
    with :? System.FormatException ->
        None


lines
|> Seq.map tryParseInt
|> Seq.fold
    (fun acc line ->
        match line with
        | None -> acc @ [ [] ]
        | Some(line) -> (List.take (acc.Length - 1) acc) @ [ (List.last acc) @ [ line ] ])
    [ [] ]
|> List.map List.sum
|> List.max
|> printfn "%d"
