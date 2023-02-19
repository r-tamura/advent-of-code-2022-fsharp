let chop (input: string) len =
    seq {
        for start in 0..len .. input.Length - 1 do
            input.[start .. start + len - 1]
    }
    |> Seq.toArray

let splitInHalf (s: string) =
    let middle: int = s.Length / 2
    let chopped = (s, middle) ||> chop

    match Array.toList chopped with
    | [ firstCompartment; seconddCompartment ] -> (firstCompartment, seconddCompartment)
    | _ -> failwith $"failed to split '%A{chopped}'"

let findDuplicatedItems (s1: string) (s2: string) : char array =
    Set.intersect (Set.ofSeq s1) (Set.ofSeq s2) |> Set.toArray

let mapCharToPoint (c: char) =
    match c with
    | c when 'a' <= c && c <= 'z' -> int c - int 'a' + 1
    | c when 'A' <= c && c <= 'Z' -> int c - int 'A' + 26 + 1
    | _ -> failwith $"'{c}' is invalid char"

let part1 lines =
    lines
    |> Array.map splitInHalf
    |> Array.map (fun t -> t ||> findDuplicatedItems)
    |> Array.concat
    |> Array.map mapCharToPoint
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
| None -> failwith "couldn't find the input data"
| Some(lines) ->
    match part with
    | "1" -> lines |> part1
    | _ -> failwith "part is '1'"
