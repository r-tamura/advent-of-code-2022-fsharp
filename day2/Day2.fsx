#load "RockPaperScissors.fsx"

open RockPaperScissors

let args = fsi.CommandLineArgs |> Array.tail

let filepath, part =
    match Array.toList args with
    | filepath :: part :: _ -> (filepath, part)
    | _ -> failwith "usage: dotnet fsi </path/to/this/program> </path//to/data/file> <1 | 2>"

let lines =
    try
        Some(System.IO.File.ReadAllLines(filepath))
    with :? System.IO.FileNotFoundException as error ->
        None

let mapCharToHand (c: string) : Hand option =
    match c with
    | "A"
    | "X" -> Hand.fromString "rock" |> Result.toOption
    | "B"
    | "Y" -> Hand.fromString "paper" |> Result.toOption
    | "C"
    | "Z" -> Hand.fromString "scissors" |> Result.toOption
    | _ -> None

let mapCharToResult (c: string) : RoundResult option =
    match c with
    | "X" -> Some Lose
    | "Y" -> Some Draw
    | "Z" -> Some Win
    | _ -> None


let tryParseRoundLineAsChars (line: string) : (char * char) option =
    line.Split(" ")
    |> Array.map (fun s ->
        match s with
        | s when s.Length = 1 -> s |> Seq.toArray |> Seq.head |> Some
        | _ -> None)
    |> fun tokens ->
        match tokens with
        | [| Some(opponent); Some(result) |] -> Some(opponent, result)
        | _ -> None


let tryParseRoundLine (line: string) : (Hand * Hand) option =
    let hands =
        line.Split(" ") |> Array.map mapCharToHand |> Seq.choose id |> Seq.toList

    if hands.Length < 2 then None else Some(hands[0], hands[1])


let calcRoundPoint (line: string) =
    line
    |> tryParseRoundLine
    |> Option.map (fun (hand1: Hand, hand2: Hand) -> (hand1, hand2, doRound hand1 hand2))
    |> Option.map (fun (_, hand2, result) -> RockPaperScissors.calcRoundPoint hand2 result)

let createHandAndResultFromStrings (hand, result) =
    ((hand.ToString() |> mapCharToHand), (result.ToString() |> mapCharToResult))
    ||> fun hand result ->
        match (hand, result) with
        | (None, _) -> failwith "couldn't parse opponent hand"
        | (_, None) -> failwith "couldn't parse expected result"
        | (Some(hand), Some(result)) -> (hand, result)


let guessHand opponent result =
    match (opponent, result) with
    | (Rock, Win)
    | (Scissors, Lose) -> Paper
    | (Rock, Lose)
    | (Paper, Win) -> Scissors
    | (Paper, Lose)
    | (Scissors, Win) -> Rock
    | (hand, Draw) -> hand

let calcRoundPoint2 (line: string) =
    line
    |> tryParseRoundLineAsChars
    |> Option.map createHandAndResultFromStrings
    |> Option.map (fun (hand, result) -> (((hand, result) ||> guessHand), result))
    |> Option.map (fun v -> v ||> RockPaperScissors.calcRoundPoint)


let part1 lines =
    lines
    |> Array.map calcRoundPoint
    |> Array.choose id
    |> Array.sum
    |> printfn "%d"

let part2 lines =
    lines
    |> Array.map calcRoundPoint2
    |> Array.choose id
    |> Array.sum
    |> printfn "%d"

match lines with
| None -> failwith "couldn't read the input file"
| Some(lines) ->
    match part with
    | "1" -> lines |> part1
    | "2" -> lines |> part2
    | _ -> failwith "part is '1' or '2'"
