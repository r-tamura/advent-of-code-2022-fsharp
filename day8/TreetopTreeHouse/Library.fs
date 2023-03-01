module TreetopTreeHouse

module Array2D =
    // https://stackoverflow.com/a/29876264/6253973
    let foldi (folder: int -> int -> 'S -> 'T -> 'S) (state: 'S) (array: 'T[,]) =
        seq {
            for x in 0 .. Array2D.length1 array - 1 do
                for y in 0 .. Array2D.length2 array - 1 do
                    yield (x, y, array.[x, y])
        }
        |> Seq.fold (fun acc (x, y, e) -> folder x y acc e) state

let private split (s: string) : char[] = Seq.toArray s

let loadInput path =
    let toString v = v.ToString()

    path
    |> System.IO.File.ReadAllLines
    |> Array.map split
    |> array2D
    |> Array2D.map toString
    |> Array2D.map int

let getRow row (heightMap: int[,]) = heightMap[row, *]

let getCol col (heightMap: int[,]) = heightMap[*, col]


let getFourDirection i j map =
    let row, col = (getRow i map, getCol j map)
    let left = (if j > 0 then row[.. j - 1] else [||]) |> Array.rev
    let right = if j < row.Length then row[j + 1 ..] else [||]
    let top = (if i > 0 then col[.. i - 1] else [||]) |> Array.rev
    let bottom = if i < col.Length then col[i + 1 ..] else [||]
    (top, right, bottom, left)

type View =
    | Blocked
    | NotBlocked

let getViewingDistance h line =
    match line with
    | [||] -> (0, NotBlocked)
    | _ ->
        line
        |> Array.tryFindIndex ((<=) h)
        |> Option.map (fun index -> (index + 1, Blocked))
        |> Option.defaultValue (line.Length, NotBlocked)

let isVisibleDirection targetHeight line =
    match getViewingDistance targetHeight line with
    | (_, Blocked) -> false
    | _ -> true

let isVisible i j (map: int[,]) =
    let targetHeight = map[i, j]
    let top, right, bottom, left = getFourDirection i j map

    [| top; right; bottom; left |]
    |> Array.map (isVisibleDirection targetHeight)
    |> Array.exists id

let calcScenicScore i j (map: int[,]) : int =
    let targetHeight = map[i, j]
    let top, right, bottom, left = getFourDirection i j map

    let folder (acc: int) (viewingDistance: int * View) =
        let distance, _ = viewingDistance
        acc * distance

    [| top; right; bottom; left |]
    |> Array.map (getViewingDistance targetHeight)
    |> Array.fold folder 1

let countVisible (heightMap: int[,]) =
    heightMap
    |> Array2D.foldi (fun i j count h -> if isVisible i j heightMap then count + 1 else count) 0

let findHighestSenicScore (map: int[,]) =
    map
    |> Array2D.foldi (fun i j max (h: int) -> Array.max [| max; (calcScenicScore i j map) |]) 0



module Part1 =
    let run (path: string) =
        path |> loadInput |> countVisible |> printfn "%A"


module Part2 =
    let run (path: string) =
        path |> loadInput |> findHighestSenicScore |> printfn "%A"
