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

    let xRangeOn y sensor =
        let beaconDistance = distanceFromBeacon sensor

        if
            y < snd sensor.position - beaconDistance
            || y > snd sensor.position + beaconDistance
        then
            // 最近ビーコンと同じ距離内に、指定された行がない
            None
        else
            // センサーのx座標
            let sensorX = fst sensor.position
            // センサーと指定行のy座標の距離
            let yDistance = abs (snd sensor.position - y)
            // 指定されたy座標上のセンサーのX軸上のスキャン範囲
            let scanRange = beaconDistance - yDistance

            let xStart = sensorX - scanRange
            let xEnd = sensorX + scanRange

            Some(xStart, xEnd)

module BeaconExclusionZone =
    let xRangesOn y sensors =
        sensors
        |> Seq.map (fun sensor -> Sensor.xRangeOn y sensor)
        |> Seq.choose id
        |> Seq.sort
        |> Seq.fold
            (fun acc range ->

                let rec replaceLastWith newList item =
                    match newList with
                    | [] -> failwith "List cannot be empty."
                    | [ x ] -> [ item ] // only one element in the list
                    | x :: xs -> x :: (replaceLastWith xs item)

                if Seq.length acc = 0 then
                    [ range ]
                else
                    let firstRange = acc.[0]
                    let accMin, _ = firstRange
                    let lastRange = List.last acc
                    let _, accMax = lastRange
                    let rangeMin, rangeMax = range

                    acc
                    |> (fun acc -> if accMax + 1 < rangeMin then acc @ [ range ] else acc)
                    |> (fun acc ->
                        let last = List.last acc
                        let lastMin, lastMax = last
                        replaceLastWith acc (lastMin, max lastMax rangeMax)))
            []

    let filterCoveredPoints y sensors ranges =
        let splitRangeAt range point =
            let rmin, rmax = range

            if point < rmin || rmax < point then [ range ]
            else if point = rmin then [ (rmin + 1, rmax) ]
            else if point = rmax then [ (rmin, rmax - 1) ]
            else [ (rmin, point - 1); (point + 1, rmax) ]

        let sensorsAndBeaconsOnY =
            sensors
            |> Seq.collect (fun sensor -> [ sensor.position; sensor.closestBeacon ])
            |> Seq.filter (fun (_, y') -> y = y')

        ranges
        |> Seq.collect (fun range ->
            // rangeMin, rangeMax
            let rmin, rmax = range

            sensorsAndBeaconsOnY
            |> Seq.fold
                (fun ranges' coveredPoint ->

                    let nextRanges' =
                        ranges' |> List.collect (fun range' -> splitRangeAt range' (fst coveredPoint))

                    nextRanges')
                [ range ])

    let countPoints ranges =
        ranges
        |> Seq.fold
            (fun acc range ->
                let min, max = range
                acc + (max - min + 1))
            0

module Part1 =
    let solve (inputFilePath: string) (y: int) =
        let isCoverdBySensorOrBeacon point =
            Seq.exists (fun sensor -> sensor.position = point || sensor.closestBeacon = point)

        let sensors = inputFilePath |> System.IO.File.ReadLines |> parse

        BeaconExclusionZone.xRangesOn y sensors
        |> BeaconExclusionZone.filterCoveredPoints y sensors
        |> BeaconExclusionZone.countPoints
        |> printfn "%A"

module Part2 =
    let solve (inputFilePath: string) rangeMax =
        let sensors = inputFilePath |> System.IO.File.ReadLines |> parse

        let positiveIntercepts =
            sensors
            |> Seq.fold
                (fun acc sensor ->
                    let d = Sensor.distanceFromBeacon sensor
                    let sx, sy = sensor.position
                    let positiveIntercept1 = sy - sx + (d + 1)
                    let positiveIntercept2 = sy - sx - (d + 1)

                    acc @ [ positiveIntercept1; positiveIntercept2 ])
                []

        let negativeIntercepts =
            sensors
            |> Seq.fold
                (fun acc sensor ->
                    let d = Sensor.distanceFromBeacon sensor
                    let sx, sy = sensor.position
                    let negativeIntercept1 = sy + sx + (d + 1)
                    let negativeIntercept2 = sy + sx - (d + 1)

                    acc @ [ negativeIntercept1; negativeIntercept2 ])
                []

        let product xs ys =
            List.collect (fun x -> List.map (fun y -> x, y) ys) xs

        product positiveIntercepts negativeIntercepts
        |> Seq.map (fun (i1, i2) -> (i1 + i2) / 2) // 交点のY座標
        |> Set.ofSeq
        |> Set.toSeq
        |> Seq.filter (fun y -> 0 <= y && y <= rangeMax)
        |> Seq.map (fun y -> (y, BeaconExclusionZone.xRangesOn y sensors))
        |> Seq.filter (fun (_, xRanges) -> xRanges |> Seq.length > 1)
        |> Seq.head
        |> (fun (y, xRanges) ->
            let r1 = xRanges.[0]
            let _, r1max = r1
            // 最初の区間と次の区間の間のX
            ((r1max + 1), y))
        |> (fun (x, y) -> ((uint64 x) * 4000000UL + (uint64 y)))
        |> printfn "%A"


let args = fsi.CommandLineArgs |> Array.tail

let inputFilePath, part, y =
    match args with
    | [| inputFilePath; "1"; y |] -> (inputFilePath, 1, int y)
    | [| _; "1" |] -> failwith "part1 requires 'y'"
    | [| inputFilePath; "2"; rangeMax |] -> (inputFilePath, 2, int rangeMax) // x: 0 - rangeMax, y: 0 - rangeMax
    | [| _; "2" |] -> failwith "part1 requires 'rangeMax'"
    | [| inputFilePath |] -> (inputFilePath, 1, 10)
    | _ -> failwith "usage: dotnet run <input file> <part>"

match part with
| 1 -> Part1.solve inputFilePath y
| 2 -> Part2.solve inputFilePath y
| _ -> failwith "part in '1' or '2'"

// y = a1x + b1
// y = a2x + b2
// 2直線の交点
// a1x + b1 = a2x + b2
// (a1 - a2)x = b2 - b1
// x = (b2 - b1) / (a1 - a2)
// a1 = 1, a2 = -1の場合
// x = (b2 - b1) / 2
