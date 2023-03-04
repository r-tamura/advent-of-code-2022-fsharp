namespace RopeBridge

module Array2DPlus =
    // https://stackoverflow.com/a/29876264/6253973
    let foldi (folder: int -> int -> 'S -> 'T -> 'S) (state: 'S) (array: 'T[,]) =
        seq {
            for x in 0 .. Array2D.length1 array - 1 do
                for y in 0 .. Array2D.length2 array - 1 do
                    yield (x, y, array.[x, y])
        }
        |> Seq.fold (fun acc (x, y, e) -> folder x y acc e) state

type Motion =
    | Up of int
    | Right of int
    | Down of int
    | Left of int

module Motion =
    let parse (s: string) : Motion =
        let items = s.Split(" ")

        match items with
        | [| direction; stepCountStr |] ->
            let stepCount = int stepCountStr

            match direction with
            | "R" -> Motion.Right(stepCount)
            | "U" -> Motion.Up(stepCount)
            | "D" -> Motion.Down(stepCount)
            | "L" -> Motion.Left(stepCount)
            | _ -> failwith $"invali direction '{direction}'"
        | _ -> failwith $"invalid motion: '{s}'"



type Position = int * int

module Position =
    let sub (p1: Position) (p2: Position) : Position =
        let row1, col1 = p1
        let row2, col2 = p2
        (row1 - row2, col1 - col2)

type State =
    { Head: Position
      Tail: Position
      Footprints: Set<Position> }

module State =

    let private visit (row, col) (footprints) = Set.add (row, col) footprints

    /// RopeBridgeの状態を初期化します。HeadとTailの開始位置は一番左下の座標になります。
    let init =

        let start = (0, 0)

        { Head = start
          Tail = start
          Footprints = visit start Set.empty }

    let followHead head tail =
        let delta = Position.sub head tail
        let row, col = tail

        match delta with
        | (0, 0) -> tail
        | (drow, dcol) when (abs drow + abs dcol) < 2 -> tail
        | (0, dcol) when dcol < -1 -> (row, col - 1)
        | (0, dcol) when dcol > 1 -> (row, col + 1)
        | (drow, 0) when drow < -1 -> (row - 1, col)
        | (drow, 0) when drow > 1 -> (row + 1, col)
        | (2, 1)
        | (1, 2) -> (row + 1, col + 1)
        | (2, -1)
        | (1, -2) -> (row + 1, col - 1)
        | (-2, 1)
        | (-1, 2) -> (row - 1, col + 1)
        | (-2, -1)
        | (-1, -2) -> (row - 1, col - 1)
        | _ -> tail


    let step motion (s: State) =
        let oneStep (motion: Motion) (s: State) =
            let nextHead =
                let row, col = s.Head

                match motion with
                | Motion.Up(_) -> (row - 1, col)
                | Motion.Right(_) -> (row, col + 1)
                | Motion.Down(_) -> (row + 1, col)
                | Motion.Left(_) -> (row, col - 1)

            let nextTail = followHead nextHead s.Tail
            let nextFootprints = visit nextTail s.Footprints

            { Head = nextHead
              Tail = nextTail
              Footprints = nextFootprints }

        let step =
            match motion with
            | Motion.Up(step)
            | Motion.Right(step)
            | Motion.Down(step)
            | Motion.Left(step) -> step

        [| 0 .. step - 1 |] |> Array.fold (fun acc _ -> oneStep motion acc) s


    let countVisited (s: State) = Set.count s.Footprints

// let toString (s: State) =
//     s.Board
//     |> Array2D.mapi (fun i j visited ->
//         match visited with
//         | true -> '#'
//         | false -> '.')
//     |> (fun b ->
//         let rowCount = Array2D.length1 b
//         [| 0 .. rowCount - 1 |] |> Array.map (fun row -> b[row, *] |> System.String))
//     |> String.concat "\n"
