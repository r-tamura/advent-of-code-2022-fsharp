module Parser

open System.Text.RegularExpressions

module StackInput =
    open SupplyStack

    type Node =
        | Crate of char
        | Empty

    let tokenizeLine (line: string) : Node array =
        let rec parse' (i: int) (tokens: Node list) : Node list =
            if i > line.Length then
                tokens
            else
                match line[i] with
                | ' ' -> parse' (i + 4) (Empty :: tokens)
                | '[' -> parse' (i + 4) (Crate line.[i + 1] :: tokens)
                | t -> failwith $"unexpected token '{t}' found"

        parse' 0 [] |> List.rev |> List.toArray


    let parse (lines: string[]) : Stack<SupplyStack.Crate> array =
        let popLast xs =
            match xs with
            | [||] -> None
            | _ -> Some((Array.last xs, xs[0 .. xs.Length - 2]))

        let (stackNumberLine, crateLines) =
            match popLast lines with
            | Some(x) -> x
            | None -> failwith "no stacks found"

        let crateNodesList = crateLines |> Array.map tokenizeLine
        let isNotEmptyString s = s <> ""

        let stackCount =
            (stackNumberLine.Split(" ") |> Array.filter isNotEmptyString).Length


        let createStack (crateNodesList: Node[][]) (i: int) : Stack<Crate> =

            let folder (i: int) (acc: Stack<Crate>) (lineNodes: Node[]) =
                match i with
                | _ when i >= lineNodes.Length -> acc
                | _ ->
                    match lineNodes.[i] with
                    | Empty -> acc
                    | Crate(c) ->
                        acc.values <- acc.values @ [ (SupplyStack.Crate c) ]
                        acc

            crateNodesList |> Array.fold (folder i) (Stack.create<Crate> ())


        seq { 0 .. stackCount - 1 }
        |> Seq.map (createStack crateNodesList)
        |> Seq.toArray

module CommandInput =
    let parse (lines: string[]) : SupplyStack.MoveCommand array =
        let COMMAND_PATTERN = @"move (\d+) from (\d+) to (\d+)"

        let parseCommand line =
            let matches = Regex.Match(line, COMMAND_PATTERN)

            match matches.Groups |> Seq.toList |> List.tail with
            | amount :: from :: moveTo :: _ -> Some(int amount.Value, int from.Value, int moveTo.Value)
            | _ -> None

        lines |> Array.map parseCommand |> Array.choose id


let parse (text: string) =

    let lines = text.Split "\n"

    let index = lines |> Array.findIndex (fun line -> line = "")

    lines
    |> Array.splitAt index
    |> fun (stacks: string[], commands: string[]) ->
        (StackInput.parse stacks, CommandInput.parse (commands |> Array.tail))
