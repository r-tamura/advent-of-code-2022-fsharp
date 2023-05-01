#r "nuget: FParsec"

open FParsec

type SensorBeacon =
    { position: int * int
      closestBeacon: int * int }

let parse lines =
    let parseLine line =
        let sensor = pstring "Sensor at x=" >>. pint32 .>> pstring ", y=" .>>. pint32

        let closestBeacon =
            pstring "closest beacon is at x=" >>. pint32 .>> pstring ", y=" .>>. pint32

        let pline =
            sensor .>> pstring ": " .>>. closestBeacon
            |>> fun (posision, closestBeacon) ->
                { position = posision
                  closestBeacon = closestBeacon }

        let result = run pline line

        match result with
        | Success(sensorBeacon, _, _) -> sensorBeacon
        | Failure(err, _, _) -> failwith err


    lines |> Seq.map parseLine

module Sensor =
    let distance from' to' =
        let x1, y1 = from'
        let x2, y2 = to'

        abs (x1 - x2) + abs (y1 - y2)

    let distanceFromBeacon sensor =
        distance sensor.position sensor.closestBeacon

    let coverHorizontalLine y sensor =
        let beaconDistance = distanceFromBeacon sensor

        if
            y < snd sensor.position - beaconDistance
            || y > snd sensor.position + beaconDistance
        then
            // 最近ビーコンと同じ距離内に、指定された行がない
            Set.empty<int * int>
        else
            // センサーのx座標
            let sensorX = fst sensor.position
            // センサーと指定行のy座標の距離
            let yDistance = abs (snd sensor.position - y)
            // 指定されたy座標上のセンサーのX軸上のスキャン範囲
            let scanRange = beaconDistance - yDistance

            let xStart = sensorX - scanRange
            let xEnd = sensorX + scanRange

            let rec coverHLine' x (filled: Set<int * int>) =
                if xEnd < x then
                    filled
                else
                    let nextFilled = Set.add (x, y) filled
                    coverHLine' (x + 1) nextFilled

            coverHLine' xStart Set.empty<int * int>

module Part1 =
    let filled = Set.empty<int * int>

    let solve (inputFilePath: string) (y: int) =
        let isCoverdBySensorOrBeacon point =
            Seq.exists (fun sensor -> sensor.position = point || sensor.closestBeacon = point)

        let sensors = inputFilePath |> System.IO.File.ReadLines |> parse

        let filled =
            sensors
            |> Seq.fold
                (fun filled sensor ->
                    let filledByASensor = Sensor.coverHorizontalLine y sensor
                    filledByASensor |> Set.fold (fun acc p -> Set.add p acc) filled)
                Set.empty

        filled
        |> Seq.filter (fun point -> snd point = y)
        |> Seq.filter (fun point -> sensors |> isCoverdBySensorOrBeacon point |> (fun b -> not b))
        |> Seq.length
        |> printfn "%A"

module Part2 =
    let solve (inputFilePath: string) = failwith "not implemented"

let args = fsi.CommandLineArgs |> Array.tail

let inputFilePath, part, y =
    match args with
    | [| inputFilePath; "1"; y |] -> (inputFilePath, 1, int y)
    | [| inputFilePath |] -> (inputFilePath, 1, 10)
    | _ -> failwith "usage: dotnet run <input file> <part>"

match part with
| 1 -> Part1.solve inputFilePath y
| 2 -> Part2.solve inputFilePath
| _ -> failwith "part in '1' or '2'"
