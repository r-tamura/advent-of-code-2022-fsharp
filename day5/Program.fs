open SupplyStack
open Parser

[<EntryPoint>]
let main argv =
    if argv.Length < 1 then
        failwith "usage: dotnet run <input file> <part>"

    let filepath = argv[0]

    let input = System.IO.File.ReadAllText(filepath)

    let (stacks: Stack<Crate> array, commands) = parse input

    // let stacks = [| Stack.fromList [ Crate 'a'; Crate 'b'; Crate 'c' ]; Stack.fromList [ Crate 'x'; Crate 'y'; Crate 'z' ] |]
    // let commands = [| (1, 1, 2); |]

    commands
    |> Array.fold (fun (stacks: Stack<Crate> array) (a, b, c) -> SupplyStackService.move a b c stacks) stacks
    |> Array.map Stack.peek
    |> Array.choose id
    |> Array.map (fun (Crate c) -> c)
    |> System.String.Concat
    |> printfn "%A"



    0
