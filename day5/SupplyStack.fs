module SupplyStack

type Crate = Crate of char

type Stack<'a> = { mutable values: 'a list }

module Stack =
    let create<'a> () : Stack<'a> = { values = [] }

    let fromList<'a> (elements: 'a list) : Stack<'a> =
        let stack = create ()
        stack.values <- elements
        stack

    let push<'a> (x: 'a) (stack: Stack<'a>) = stack.values <- x :: stack.values

    let pop<'a> (stack: Stack<'a>) : 'a option =
        match stack.values with
        | [] -> None
        | x :: xs ->
            stack.values <- xs
            Some x


    let peek<'a> (stack: Stack<'a>) : 'a option =
        match stack.values with
        | [] -> None
        | x :: xs -> Some x


type MoveCommand = int * int * int

module SupplyStackService =

    let move amount from moveto (stacks: Stack<Crate> array) =
        for _ in [ 0 .. amount - 1 ] do
            let crate = Stack.pop (stacks[from - 1])

            match crate with
            | None -> failwith $"StackEmptyError: stack '{from}' is empty"
            | Some(crate: Crate) -> Stack.push crate stacks[moveto - 1]

        stacks

    let moveSet amount from moveto (stacks: Stack<Crate> array) =
        [ 0 .. amount - 1 ]
        |> Seq.map (fun _ -> Stack.pop (stacks[from - 1]))
        |> Seq.choose id
        |> Seq.rev
        |> Seq.iter (fun crate -> Stack.push crate stacks[moveto - 1])

        stacks
