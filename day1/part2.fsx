let args = fsi.CommandLineArgs |> Array.tail

if args.Length < 1 then
    failwith "You need to specify input data file path"


let filepath = args[0]

let lines =
    try
        Some(System.IO.File.ReadAllLines(filepath))
    with :? System.IO.FileNotFoundException ->
        None


let tryParseInt (str: string) : Option<int> =
    try
        str |> int |> Some
    with :? System.FormatException ->
        None

let groupByEmptyLine acc line =
    match line with
    | None -> acc @ [ [] ]
    | Some(line) -> (List.take (acc.Length - 1) acc) @ [ (List.last acc) @ [ line ] ]

let outputAnswer (xs: int option) : unit =
    match xs with
    | Some(xs) -> printfn "Total carolies of the top three Elves: %d" xs
    | None -> eprintfn "An error occured"

lines
|> Option.map (Array.map tryParseInt)
|> Option.map (Array.fold groupByEmptyLine [ [] ])
|> Option.map (List.map List.sum)
|> Option.map List.sortDescending
|> Option.map (List.take 3)
|> Option.map List.sum
|> outputAnswer
