#r "nuget: Validus"

open Validus


type Assignment = { startIndex: int; endIndex: int }

module Assignment =
    let parse (s: string) : Assignment =
        let l = s.Split("-")
        let startIndex = l[0]
        let endIndex = l[1]

        { startIndex = int startIndex
          endIndex = int endIndex }

    let contains (a1: Assignment) (a2: Assignment) : bool =
        /// <summary>check if the first argument contains the second argument</summary>
        a1.startIndex <= a2.startIndex && a2.endIndex <= a1.endIndex

    let overrapped (a1: Assignment) (a2: Assignment) : bool =
        // a1 ..345.
        // a2 ...456
        (a1.startIndex <= a2.startIndex && a2.startIndex <= a1.endIndex)
        // a1 ..345.
        // a2 .234..
        || (a1.startIndex <= a2.endIndex && a2.endIndex <= a1.endIndex)
        // a1 ..3..
        // a2 .23..
        || (a2.startIndex <= a1.startIndex && a1.startIndex <= a2.endIndex)
        || (a2.startIndex <= a1.endIndex && a1.endIndex <= a2.endIndex)

    type CompareResult =
        | Contains
        | Contained
        | Overwrapped
        | NoOverwrap

    let compare (a1: Assignment) (a2: Assignment) : CompareResult =
        if contains a1 a2 then Contains
        else if contains a2 a1 then Contained
        else if overrapped a1 a2 then Overwrapped
        else NoOverwrap

let getAssignmentPair (line: string) : Assignment * Assignment =
    line.Split(",") |> Array.map Assignment.parse |> (fun l -> (l[0], l[1]))

let boolToInt b = if b then 1 else 0

let isAssignmentPairOverrapped =
    getAssignmentPair >> (fun pair -> pair ||> Assignment.overrapped) >> boolToInt


let part1 lines =
    lines
    |> Array.map getAssignmentPair
    |> Array.map (fun pair ->
        let result = pair ||> Assignment.compare
        result = Assignment.Contains || result = Assignment.Contained)
    |> Array.map boolToInt
    |> Array.sum
    |> printfn "%A"

let part2 lines =
    lines |> Array.map isAssignmentPairOverrapped |> Array.sum |> printfn "%A"


let args = fsi.CommandLineArgs |> Array.tail

let datapath, part =
    match args |> Array.truncate 2 with
    | [| datapath; part |] -> (datapath, part)
    | _ -> failwith "usage: dotnet fsi <program.fsi> <data-file> <probrem part no>"

let lines =
    try
        Some(System.IO.File.ReadAllLines(datapath))
    with
    | :? System.IO.FileNotFoundException
    | :? System.IO.DirectoryNotFoundException -> None

match lines with
| None -> failwith "could not open input file"
| Some(lines) ->
    match part with
    | "1" -> part1 lines
    | "2" -> part2 lines
    | _ -> failwith "no part found"
