let areDifferentAllChars (s: string) : bool =
    let uniqueChars = s |> Set.ofSeq |> List.ofSeq
    s.Length = uniqueChars.Length


let findStartOfPacketMarker (stream: string) (packageLength: int) =
    let rec findStartOfPacketMarker' i =
        let cand = stream[i .. i + packageLength - 1]

        if areDifferentAllChars cand then
            (i, cand)
        else
            findStartOfPacketMarker' (i + 1)

    findStartOfPacketMarker' 0


let getEndOfPacketMarkerIndex (start: int) (packet: string) : int = start + packet.Length

let MARKER_PACKET_LENGTH_PART1 = 4
let MARKER_PACKET_LENGTH_PART2 = 14

let part1 stream =
    (stream, MARKER_PACKET_LENGTH_PART1)
    ||> findStartOfPacketMarker
    ||> getEndOfPacketMarkerIndex
    |> printfn "%A"

let part2 stream =
    (stream, MARKER_PACKET_LENGTH_PART2)
    ||> findStartOfPacketMarker
    ||> getEndOfPacketMarkerIndex
    |> printfn "%A"

[<EntryPoint>]
let main argv =
    let filepath, part =
        match argv with
        | [| filepath; part |] -> (filepath, part)
        | _ -> failwith "usage: dotnet run <filepath> <part>"


    let stream = System.IO.File.ReadAllText filepath

    match part with
    | "1" -> part1 stream
    | "2" -> part2 stream
    | _ -> failwith "part is '1' or '2'"

    0
