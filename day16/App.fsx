open System.Text.RegularExpressions

open System.Collections.Generic

type Valve =
    { name: string
      rate: int
      tunnels: string[]
      isOpen: bool }

let parse inputFile =
    let parseLine (line: string) =
        // Valve AA has flow rate=0; tunnels lead to valves DD, II, BB
        let name = line.Split " " |> (fun s -> Array.get s 1)

        let rate =
            line.Split ";"
            |> (fun s -> Array.get s 0)
            |> (fun s -> s.Split "=")
            |> (fun s -> Array.get s 1)
            |> int

        let tunnels =
            line.Split ";"
            |> (fun s -> Array.get s 1)
            |> (fun s -> s.Split " ")
            |> Array.map (fun s ->
                let upperTwo = Regex(@"[A-Z]{2}")
                (upperTwo.Match s).Value)
            |> Array.filter (fun s -> s <> "")

        let isOpen = false

        { name = name
          rate = rate
          tunnels = tunnels
          isOpen = isOpen }

    System.IO.File.ReadAllLines inputFile |> Array.map parseLine




let initDistancesFrom start valves =
    let distances =
        valves
        |> List.filter (fun v -> v.name = start || v.rate > 0)
        |> List.fold
            (fun (distsFromTo: Map<string, Map<string, int>>) valve ->
                let mutable visited = Set.ofList [ valve.name ]
                let queue = new Queue<int * Valve>()
                queue.Enqueue(0, valve)
                let mutable distsTo = Map.ofList [ (valve.name, 0); ("AA", 0) ]

                while queue.Count > 0 do
                    let dist, valve = queue.Dequeue()

                    for neighbor in valve.tunnels do
                        let neighborValve = List.find (fun v -> v.name = neighbor) valves

                        if not (Set.exists (fun v -> neighbor = v) visited) then
                            visited <- Set.add neighbor visited

                            if neighborValve.rate > 0 then
                                distsTo <- Map.add neighbor (dist + 1) distsTo

                            queue.Enqueue((dist + 1), neighborValve)

                distsTo <- Map.remove valve.name distsTo

                if valve.name <> "AA" then
                    distsTo <- Map.remove "AA" distsTo

                Map.add valve.name distsTo distsFromTo)
            Map.empty

    distances

let mutable cache = Map.empty

let rec dfs time valve (opened: int) distFromTo valves bits =
    let cached = Map.tryFind (time, valve, opened) cache

    match cached with
    | Some(v) -> v
    | None ->
        distFromTo
        |> Map.find valve.name
        |> Map.keys
        |> Seq.fold
            (fun maxValue neighbor ->
                let neighborBit = bits |> Map.find neighbor
                let isOpened = opened &&& neighborBit

                if isOpened > 0 then
                    maxValue
                else
                    let dist = distFromTo |> Map.find valve.name |> Map.find neighbor
                    let remainingTime = time - (dist + 1) // move (dist) + open (1)

                    if remainingTime > 0 then
                        let neighborValve = List.find (fun v -> v.name = neighbor) valves

                        let candidate =
                            (dfs remainingTime neighborValve (opened ||| neighborBit) distFromTo valves bits)
                            + neighborValve.rate * remainingTime

                        let next = max maxValue candidate
                        cache <- Map.add (time, valve, opened) next cache
                        next
                    else
                        maxValue)
            0


module Part1 =
    let solve inputFile =
        let valves = parse inputFile |> Array.toList
        let start = "AA"

        let distFromTo = valves |> initDistancesFrom start

        let startValve = List.find (fun v -> v.name = start) valves

        let nonempty =
            distFromTo |> Map.keys |> Seq.filter (fun k -> k <> start) |> Seq.toList

        let bits = nonempty |> List.mapi (fun i v -> v, (1 <<< i)) |> Map.ofList

        dfs 30 startValve 0 distFromTo valves bits |> printfn "%d"


module Part2 =
    let solve inputFile =
        let valves = parse inputFile |> Array.toList
        let start = "AA"
        let distFromTo = valves |> initDistancesFrom start

        let startValve = List.find (fun v -> v.name = start) valves

        let nonempty =
            distFromTo |> Map.keys |> Seq.filter (fun k -> k <> start) |> Seq.toList

        let bits = nonempty |> List.mapi (fun i v -> v, (1 <<< i)) |> Map.ofList

        let maxOpened = (1 <<< (List.length nonempty)) - 1
        let mutable maxValue = 0

        seq { 0 .. (maxOpened / 2) }
        |> Seq.fold
            (fun maxValue opened ->
                let cand =
                    (dfs 26 startValve opened distFromTo valves bits)
                    + (dfs 26 startValve (maxOpened ^^^ opened) distFromTo valves bits)

                max maxValue cand)
            0
        |> printfn "%d"

let args = fsi.CommandLineArgs |> Array.tail

let filepath, part =
    match args with
    | [| filepath |] -> filepath, 1
    | [| filepath; "1" |] -> filepath, 1
    | [| filepath; "2" |] -> filepath, 2
    | _ -> failwith "usage: dotnet fsi App.fsx <input file>"


match part with
| 1 -> Part1.solve filepath
| 2 -> Part2.solve filepath
| _ -> failwith "unknown part: expected 1 or 2"
