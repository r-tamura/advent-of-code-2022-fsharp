namespace RegolithReservoir

type CellState =
    | A // Air
    | S // Sand
    | R // Rock


type Cave = { leftSide: int; cells: CellState[,] }

type LocalPosition = int * int
type GlobalPosition = int * int
type RockPath = GlobalPosition list

open FParsec

module Array2DPlus =
    let fold<'T> fn initial (arr: 'T[,]) =
        let mutable acc = initial

        for y in 0 .. Array2D.length1 arr - 1 do
            for x in 0 .. Array2D.length2 arr - 1 do
                acc <- fn x y acc arr.[y, x]

        acc

module Parser =
    let parse (input: string) : RockPath list =
        let whitespaces = many (pchar ' ')
        let position = pint32 .>> pchar ',' .>> whitespaces .>>. pint32
        let arrow = whitespaces >>. pstring "->" .>> whitespaces

        let rockPath = sepBy position arrow
        let rockPaths = sepBy rockPath newline

        match run rockPaths input with
        | Success(result, _, _) -> result |> List.filter (fun path -> path.Length > 0)
        | Failure(msg, _, _) -> failwith msg

module RockPath =
    let boundingRect (path: RockPath) =
        let minX = path |> List.map fst |> List.min
        let minY = path |> List.map snd |> List.min
        let maxX = path |> List.map fst |> List.max
        let maxY = path |> List.map snd |> List.max
        (minX, minY), (maxX, maxY)

module Cave =
    let SAND_FLOWING_POINT = GlobalPosition(500, 0)

    let toStr cave =
        let cellToStr (cell: CellState) =
            match cell with
            | A -> "."
            | S -> "o"
            | R -> "#"

        let rowToStr (row: CellState[]) = row |> Array.map cellToStr

        let mutable rows = []

        for y in 0 .. Array2D.length1 cave.cells - 1 do
            let row = cave.cells.[y, *]
            let strRow = rowToStr row
            rows <- rows @ [ String.concat "" strRow ]

        // Join rows by newline
        rows |> String.concat "\n"

    let debug cave = printfn "%s" (toStr cave)

    let l2g (localPosition: LocalPosition) (cave: Cave) =
        let localX, y = localPosition
        let globalX = cave.leftSide + localX
        GlobalPosition(globalX, y)

    let g2l (globalPosition: GlobalPosition) (cave: Cave) =
        let globalX, (y: int) = globalPosition
        let localX = globalX - cave.leftSide
        LocalPosition(localX, y)

    let set (state) (pos: GlobalPosition) (cave: Cave) =
        let (localX, y) = g2l pos cave

        if localX < 0 || Array2D.length2 cave.cells <= localX then
            printfn "global %A" pos
            printfn "left side: %A" cave.leftSide
            failwith (sprintf "out of range (x): %A" localX)

        if y < 0 || Array2D.length1 cave.cells <= y then
            printfn "global %A" pos
            failwith (sprintf "out of range (y): cave height = %A, got %A" (Array2D.length1 cave.cells) y)

        cave.cells.[y, localX] <- state


    let setLocal (state: CellState) (pos: LocalPosition) (cave: Cave) =
        let (localX, y) = pos
        cave.cells.[y, localX] <- state

    let setSand (pos: GlobalPosition) (cave: Cave) =
        let x, y = g2l pos cave
        set S pos cave

    type CreateCaveOption =
        | EndlessVoid
        | InfiniteHorizontalLine of (int) // how far from the bottom of the cave

    let create (opts: CreateCaveOption) (rockPaths: RockPath list) : Cave =
        let getMostRight = List.maxBy (fun (x, _) -> x)
        let getMostLeft points = points |> List.minBy (fun (x, _) -> x)
        let getBottom points = points |> List.maxBy (fun (_, y) -> y)

        let rects = rockPaths |> List.map RockPath.boundingRect
        let leftEdge = rects |> List.map fst |> getMostLeft
        let rightEdge = rects |> List.map snd |> getMostRight
        let bottomEdge = rects |> List.map snd |> getBottom
        // width + 2 (left, right)
        // height
        // - part2: two plus the highest y coordinate of any point in your scan
        let height =
            match opts with
            | EndlessVoid -> snd bottomEdge + 1
            | InfiniteHorizontalLine distance -> snd bottomEdge + 1 + distance
        // let width = fst rightEdge - fst leftEdge + 1 + (margin * 2)
        let width = (height + 1) * 2 - 1

        let mutable cells = Array2D.init height width (fun _ _ -> A)

        let leftEdgeX = fst leftEdge
        let rightEdgeX = fst rightEdge
        let flowingPointX = fst SAND_FLOWING_POINT
        // printfn "leftEdge: %A" leftEdgeX
        // printfn "rightEdge: %A" rightEdgeX
        // printfn "path width: %A" (rightEdgeX - leftEdgeX)
        // printfn "cell width: %A" width
        // printfn "cell height: %A" height

        let cave =
            { leftSide = fst leftEdge - (leftEdgeX - (flowingPointX - width / 2))
              cells = cells }

        for path in rockPaths do
            for endIndex in 1 .. path.Length - 1 do
                let starIndex = endIndex - 1
                let start' = path.[starIndex]
                let end' = path.[endIndex]

                if fst start' = fst end' then
                    let start'', end'' =
                        if snd start' < snd end' then
                            (start', end')
                        else
                            (end', start')

                    let mutable y = snd start''

                    while y <= snd end'' do
                        let x = fst start''
                        set R (x, y) cave
                        y <- y + 1
                else
                    let start'', end'' =
                        if fst start' < fst end' then
                            (start', end')
                        else
                            (end', start')

                    let mutable x = fst start''

                    while x <= fst end'' do
                        let localY = snd start''
                        set R (x, localY) cave
                        x <- x + 1

        match opts with
        | InfiniteHorizontalLine distance ->
            let bottomIndex = Array2D.length1 cave.cells - 1

            for x in 0 .. Array2D.length2 cave.cells - 1 do
                setLocal R (x, bottomIndex) cave
        | _ -> ()

        cave

    let createDrop (cave: Cave) =
        let globalX, y = SAND_FLOWING_POINT
        let localX = globalX - cave.leftSide
        LocalPosition(localX, y)

    type FalldownOneStepResult =
        | Drop of LocalPosition
        | Stop of LocalPosition
        | Loop

    type FalldownResult =
        | Hit of GlobalPosition
        | Void

    let drop (cave: Cave) =
        let drop = createDrop cave

        let falldownOneStep (cave: Cave) (localPos: LocalPosition) =
            let x, y = localPos
            let bottomIndex = Array2D.length1 cave.cells - 1

            if y >= bottomIndex then
                Loop
            else if x = 0 then
                Loop
            else if x = Array2D.length2 cave.cells - 1 then
                Loop
            else
                let below = cave.cells.[y + 1, x]
                let left = cave.cells.[y + 1, x - 1]
                let right = cave.cells.[y + 1, x + 1]

                if below = A then Drop(x, y + 1)
                else if left = A then Drop(x - 1, y + 1)
                else if right = A then Drop(x + 1, y + 1)
                else Stop localPos

        let rec falldown (cave: Cave) (pos: LocalPosition) =
            match falldownOneStep cave pos with
            | Drop nextPos -> falldown cave nextPos
            | Stop nextLocalPos ->
                let localX, y = nextLocalPos
                let nextGlobalPos = l2g nextLocalPos cave
                Hit nextGlobalPos
            | Loop -> Void


        let dropResult = falldown cave drop

        match dropResult with
        | Hit gPos -> setSand gPos cave
        | Void -> ()

        cave, dropResult


    let dropUnitsOfSandUntilFlowingIntoVoid cave =
        let rec dropUnitsOfSandUntilFlowingIntoVoid' (cave: Cave) (sandPositions) =
            let cave', dropResult = drop cave

            match dropResult with
            | Hit gpos ->
                let sandPositions' = sandPositions @ [ gpos ]
                dropUnitsOfSandUntilFlowingIntoVoid' cave' sandPositions'
            | Void -> (cave', sandPositions)

        dropUnitsOfSandUntilFlowingIntoVoid' cave []


    let dropUnitsOfSandUntilAUnitOfSandComesToRestAtTheFlowingPoint cave =
        let rec dropUnitsOfSandUntilAUnitOfSandComesToRestAtTheFlowingPoint' (cave: Cave) (sandPositions) =
            let cave', dropResult = drop cave

            match dropResult with
            | Hit gpos ->
                if gpos = SAND_FLOWING_POINT then
                    cave'
                else
                    let sandPositions' = sandPositions @ [ gpos ]
                    dropUnitsOfSandUntilAUnitOfSandComesToRestAtTheFlowingPoint' cave' sandPositions'
            | Void ->
                debug cave'
                failwith "should not reach here"

        dropUnitsOfSandUntilAUnitOfSandComesToRestAtTheFlowingPoint' cave []

    let countSand (cave: Cave) =
        Array2DPlus.fold
            (fun _ _ acc cell ->
                match cell with
                | S -> acc + 1
                | _ -> acc)
            0
            cave.cells
