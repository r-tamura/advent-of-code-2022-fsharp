module HillClimbing

let read = System.IO.File.ReadAllLines

type HeightMap = WayPoint[,]

and WayPoint =
    | Start
    | End
    | Middle of uint

type Point = int * int
type PathList = Point[]


module Parser =
    let MIN_HEIGHT = 0u
    let MAX_HEIGHT = 25u

    let private char2height (c: char) : WayPoint =
        match c with
        | 'S' -> Start
        | 'E' -> End
        | _ -> Middle((uint c) - (uint 'a'))

    let parse lines =
        lines |> List.map (Seq.map char2height) |> array2D

module Point =
    let (+) (p1: Point) (p2: Point) =
        let x1, y1 = p1
        let x2, y2 = p2
        (x1 + x2, y1 + y2)

    let get<'T> (board: 'T[,]) (p: Point) : 'T =
        let i, j = p
        board[i, j]

module HeightMap =

    let getHeight (position: Point) (board: HeightMap) =
        let i, j = position

        match board[int i, int j] with
        | Start -> 0u
        | End -> 25u
        | Middle h -> h

    let find (finder) (board: HeightMap) =
        let nextPoint p =
            let i, j = p
            let m = Array2D.length2 board
            (i * m + j) |> (+) 1 |> (fun c -> (c / m, c % m))

        let rec find' p =
            let i, j = p

            match finder board[i, j] with
            | true -> p
            | false -> find' (nextPoint p)

        find' (0, 0)


    let private canClimbTo (board: HeightMap) from' to' =
        let fromHeight = getHeight from' board
        let toHeight = getHeight to' board
        (fromHeight + 1u) >= toHeight

    let private getAllNeighbors p board =
        let n, m = Array2D.length1 board, Array2D.length2 board

        [ (-1, 0); (0, 1); (1, 0); (0, -1) ]
        |> List.map (Point.(+) p)
        |> List.filter (fun p ->
            let i, j = p
            0 <= i && i < n && 0 <= j && j < m)

    let neighbors p (board: HeightMap) =
        getAllNeighbors p board |> List.filter (canClimbTo board p)

type VisitState =
    | NotVisited
    | Visiting
    | Visited

open System.Collections.Generic

module Array2D =
    let get (p: Point) (array: 'T[,]) =
        let i, j = p
        array[i, j]

    let set (p: Point) (v: 'T) (array: 'T[,]) =
        let i, j = p
        array[i, j] <- v

let breathFirstSearch (board: WayPoint[,]) (s: Point) =
    let n, m = Array2D.length1 board, Array2D.length2 board
    let mutable pred = Array2D.init n m (fun _ _ -> None)
    let mutable dist = Array2D.init n m (fun _ _ -> Microsoft.FSharp.Core.int.MaxValue)
    let mutable visited = Array2D.init n m (fun _ _ -> NotVisited)
    let q = new Queue<Point>()

    let visiting (p: Point) =
        let i, j = p
        visited[i, j] <- Visiting

    let setDist p v =
        let i, j = p
        dist[i, j] <- v


    visiting s
    setDist s 0

    q.Enqueue(s)

    let rec loop () =
        let found, u = q.TryDequeue()


        match (found, u) with
        | false, u -> ()
        | true, u ->
            let ui, uj = u

            for v in HeightMap.neighbors u board do
                let vi, vj = v

                if visited[vi, vj] = NotVisited then
                    dist[vi, vj] <- dist[ui, uj] + 1
                    pred[vi, vj] <- Some u
                    visited[vi, vj] <- Visiting
                    q.Enqueue(v)

            visited[ui, uj] <- Visited
            loop ()

    loop ()

    dist, pred


let fewestStepCount (board: HeightMap) =
    let start = HeightMap.find (fun h -> h = Start) board
    let d, _ = breathFirstSearch board start
    let end' = HeightMap.find (fun h -> h = End) board

    let ei, ej = end'
    d[ei, ej]
