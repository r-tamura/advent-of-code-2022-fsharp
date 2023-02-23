open SupplyStack
open Parser

let peekTopOfAllStacks stacks =
    stacks
    |> Array.map Stack.peek
    |> Array.choose id
    |> Array.map (fun (Crate c) -> c)
    |> System.String.Concat

let part1 stacks commands =
    commands
    |> Array.fold (fun (stacks: Stack<Crate> array) (a, b, c) -> SupplyStackService.move a b c stacks) stacks
    |> peekTopOfAllStacks
    |> printfn "%A"

let part2 stacks commands =
    commands
    |> Array.fold (fun (stacks: Stack<Crate> array) (a, b, c) -> SupplyStackService.moveSet a b c stacks) stacks
    |> peekTopOfAllStacks
    |> printfn "%A"

[<EntryPoint>]
let main argv =
    if argv.Length < 2 then
        failwith "usage: dotnet run <input file> <part>"

    let filepath = argv[0]
    let part = argv[1]

    let input = System.IO.File.ReadAllText(filepath)

    let (stacks: Stack<Crate> array, commands) = parse input

    // let stacks = [| Stack.fromList [ Crate 'a'; Crate 'b'; Crate 'c' ]; Stack.fromList [ Crate 'x'; Crate 'y' 'Crate cz' ; |]
    // let commands = [| (1, 1, 2); |]

    match part with
    | "1" -> part1 stacks commands
    | "2" -> part2 stacks commands
    | _ -> failwith "part is '1' or '2'"

    0
