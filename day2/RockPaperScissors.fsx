module RockPaperScissors

type Hand =
    | Rock
    | Paper
    | Scissors

module Hand =
    let fromString (rawHand: string) : Result<Hand, string> =
        match rawHand with
        | "rock" -> Ok(Rock)
        | "paper" -> Ok(Paper)
        | "scissors" -> Ok(Scissors)
        | _ -> Error(sprintf "invalid hand: '%s'" rawHand)

    let getPoint (hand: Hand) : int =
        match hand with
        | Rock -> 1
        | Paper -> 2
        | Scissors -> 3

type RoundResult =
    | Win
    | Lose
    | Draw

module RoundResult =
    let getPoint (result: RoundResult) : int =
        match result with
        | Win -> 6
        | Draw -> 3
        | Lose -> 0


let doRound (h1: Hand) (h2: Hand) : RoundResult =
    // 第一引数の手に対して第二引数の手で得られる勝敗を判定します
    match (h1, h2) with
    | (Rock, Paper) -> Win
    | (Rock, Scissors) -> Lose
    | (Paper, Rock) -> Lose
    | (Paper, Scissors) -> Win
    | (Scissors, Rock) -> Win
    | (Scissors, Paper) -> Lose
    | _ -> Draw

let calcRoundPoint (hand: Hand) (result: RoundResult) : int =
    (Hand.getPoint hand) + (RoundResult.getPoint result)
