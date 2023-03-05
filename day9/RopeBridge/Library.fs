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
    { Knots: Position[]
      Footprints: Set<Position> }

module State =

    let private visit (row, col) (footprints) = Set.add (row, col) footprints

    /// RopeBridgeの状態を初期化します。HeadとTailの開始位置は一番左下の座標になります。
    let init knots =
        let initialFootprints = visit (Array.last knots) Set.empty

        { Knots = knots
          Footprints = initialFootprints }


    let followAhead k1 k2 =
        let delta = Position.sub k1 k2
        let row, col = k2

        match delta with
        | (0, 0) -> k2
        | (drow, dcol) when abs drow < 2 && abs dcol < 2 -> k2
        | (0, dcol) when dcol < -1 -> (row, col - 1)
        | (0, dcol) when dcol > 1 -> (row, col + 1)
        | (drow, 0) when drow < -1 -> (row - 1, col)
        | (drow, 0) when drow > 1 -> (row + 1, col)
        | (2, 1)
        | (1, 2)
        | (2, 2) -> (row + 1, col + 1)
        | (2, -1)
        | (1, -2)
        | (2, -2) -> (row + 1, col - 1)
        | (-2, 1)
        | (-1, 2)
        | (-2, 2) -> (row - 1, col + 1)
        | (-2, -1)
        | (-1, -2)
        | (-2, -2) -> (row - 1, col - 1)
        | _ -> failwith $"unexpected relation: {k1} {k2} d {delta}"


    let step motion (s: State) =
        let oneStep (motion: Motion) (s: State) =
            let nextHead =
                let row, col = s.Knots[0]

                match motion with
                | Motion.Up(_) -> (row - 1, col)
                | Motion.Right(_) -> (row, col + 1)
                | Motion.Down(_) -> (row + 1, col)
                | Motion.Left(_) -> (row, col - 1)

            let nextKnots =
                s.Knots[1..]
                |> List.ofArray
                |> List.fold
                    (fun (nextKnots: Position list) (knot: Position) ->
                        List.last nextKnots
                        |> (fun lastKnot -> followAhead lastKnot knot)
                        |> (fun nextKnot -> List.append nextKnots [ nextKnot ]))
                    [ nextHead ]
                |> List.toArray

            let nextTail = Array.last nextKnots
            let nextFootprints = visit nextTail s.Footprints

            { Knots = nextKnots
              Footprints = nextFootprints }


        let step =
            match motion with
            | Motion.Up(step)
            | Motion.Right(step)
            | Motion.Down(step)
            | Motion.Left(step) -> step

        [| 0 .. step - 1 |] |> Array.fold (fun acc _ -> oneStep motion acc) s


    let countVisited (s: State) = Set.count s.Footprints

    type PointStatus =
        | Mounted of int
        | Visited
        | Clean

    let toString (s: State) =
        let minRow, maxRow, minCol, maxCol =
            [| s.Knots; Set.toArray s.Footprints |]
            |> Array.concat
            |> Array.unzip
            |> (fun (rows, cols) ->
                ((rows |> Array.min), (rows |> Array.max), (cols |> Array.min), (cols |> Array.max)))

        let height = maxRow - minRow
        let width = maxCol - minCol

        let length = (max height width) + 3


        let folder (board: PointStatus[,]) (row, col) =
            board[row, col] <- Visited
            board

        let applyKnots (board: PointStatus[,]) (index, (row, col)) =
            board[row, col] <- Mounted index
            board

        let transformCoordinate minRow maxRow minCol (row, col) = (maxRow + (length - 1) - row, col)

        let log v = printfn "%A" v; v

        let transformedFootprints = s.Footprints |> Set.map (transformCoordinate minRow maxRow minCol)
        let transformedKnots = s.Knots |> Array.map (transformCoordinate minRow maxRow minCol)
        let board =
            Array2D.create length length Clean
            |> (fun board -> Set.fold folder board transformedFootprints)
            |> (fun board ->
                ([| 0 .. s.Knots.Length - 1 |], transformedKnots)
                ||> Array.zip
                |> Array.rev
                |> Array.fold applyKnots board)

        let oneDigitIntToChar i = char (48 + i)

        board
        |> Array2D.mapi (fun i j status ->
            match status with
            | Mounted 0 -> 'H'
            | Mounted i -> oneDigitIntToChar i
            | Visited -> '#'
            | Clean -> '.')
        |> (fun b ->
            let rowCount = Array2D.length1 b
            [| 0 .. rowCount - 1 |] |> Array.map (fun row -> b[row, *] |> System.String))
        |> String.concat "\n"
