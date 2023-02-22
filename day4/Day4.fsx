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
        /// check if the first argument contains the second argument
        a1.startIndex <= a2.startIndex && a2.endIndex <= a1.endIndex

    type CompareResult =
        | Contains
        | Contained
        | Intersect
        | NotIntersect

    let compare (a1: Assignment) (a2: Assignment) : CompareResult =
        if contains a1 a2 then
            Contains
        else if contains a2 a1 then
            Contained
        else if
            a1.startIndex <= a2.startIndex && a2.startIndex <= a1.endIndex
            || a1.startIndex <= a2.endIndex && a1.endIndex <= a2.endIndex
        then
            Intersect
        else
            NotIntersect

let getAssignmentPair (line: string) : Assignment * Assignment =
    line.Split(",") |> Array.map Assignment.parse |> (fun l -> (l[0], l[1]))

let part1 lines =
    lines
    |> Array.map getAssignmentPair
    |> Array.map (fun pair ->
        let result = pair ||> Assignment.compare
        result = Assignment.Contains || result = Assignment.Contained)
    |> Array.map (fun isContaining ->
        match isContaining with
        | true -> 1
        | false -> 0)
    |> Array.sum
    |> printfn "%A"


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
    | "2" -> failwith "not implemented yet"
    | _ -> failwith "no part found"
