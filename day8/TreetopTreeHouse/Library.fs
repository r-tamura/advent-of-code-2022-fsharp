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
    path |> System.IO.File.ReadAllLines |> Array.map split |> array2D

let getRow row (heightMap: char[,]) = heightMap[row, *]

let getCol col (heightMap: char[,]) = heightMap[*, col]


let isVisible i j (heightMap: char[,]) =
    let targetHeight = int heightMap[i, j]
    let row, col = (getRow i heightMap, getCol j heightMap)
    let left = if j > 0 then row[.. j - 1] else [||]
    let right = if j < row.Length then row[j + 1 ..] else [||]
    let top = if i > 0 then col[.. i - 1] else [||]
    let bottom = if i < col.Length then col[i + 1 ..] else [||]

    let isVisibleDirection targetHeight line =
        match line with
        | [||] -> true
        | _ -> line |> Array.map (int >> (>) targetHeight) |> Array.forall id


    [| left; right; top; bottom |]
    |> Array.map (isVisibleDirection targetHeight)
    |> Array.exists id





let countVisible (heightMap: char[,]) =
    heightMap
    |> Array2D.foldi (fun i j count h -> if isVisible i j heightMap then count + 1 else count) 0

module Part1 =
    let run (path: string) =
        path |> loadInput |> countVisible |> printfn "%A"
