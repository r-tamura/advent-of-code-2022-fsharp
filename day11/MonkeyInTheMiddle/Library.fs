namespace MonkeyInTheMiddle


type Operator =
    | Add
    | Multiply

type Operand =
    | Literal of uint64
    | Variable

type MonkeyId = int
type WorryLevel = uint64

type Monkey =
    { MonkeyId: int
      Items: WorryLevel list
      Operate: Operator * Operand
      Pick: uint64 * int * int
      Stats: Stats }

and Stats = InspectCount
and InspectCount = uint64

module Monkey =

    let init id items operate pick =
        { MonkeyId = id
          Items = items
          Operate = operate
          Pick = pick
          Stats = 0UL }

    let private bored level = (/) level 3UL

    let private operate' level (monkey: Monkey) =
        let operator, operand = monkey.Operate
        let old = level

        let evaluated =
            match operand with
            | Variable -> old
            | Literal v -> v

        match operator with
        | Add -> old + evaluated
        | Multiply -> old * evaluated

    let operate index adjuster (monkey: Monkey) =
        let old = monkey.Items[index]
        let new' = operate' old monkey |> adjuster

        { monkey with
            Items = monkey.Items[.. index - 1] @ [ new' ] @ monkey.Items[index + 1 ..] }

    let choose index (monkey: Monkey) =
        let level = monkey.Items[index]
        let cond, then', else' = monkey.Pick

        let receiver =
            match (level % cond) = (uint64 0) with
            | true -> then'
            | false -> else'

        (receiver, monkey)

    let private incrementInspectCount monkey =
        { monkey with
            Stats = monkey.Stats + 1UL }


    let inspect index adjuster (monkey: Monkey) =
        monkey |> incrementInspectCount |> operate index adjuster |> choose index

    let inspectAll adjuster (monkey: Monkey) =
        let itemCount = monkey.Items.Length

        [ 0 .. itemCount - 1 ]
        |> List.mapFold (fun prevMonkey index -> inspect index adjuster prevMonkey) monkey

    let pickItem (monkey: Monkey) =
        match monkey.Items with
        | [] -> failwith $"{monkey.MonkeyId}'s items should not be empty"
        | item :: items -> (item, { monkey with Items = items })

    let pushItem item (monkey: Monkey) =
        { monkey with
            Items = monkey.Items @ [ item ] }


module MonkeyCollection =
    let throw from' to' (monkeys: Monkey list) =
        let sender = monkeys[from']
        let reciever = monkeys[to']
        let item, sender' = Monkey.pickItem sender
        let reciever' = Monkey.pushItem item reciever

        match sender.Items with
        | [] -> failwith $"{sender.MonkeyId}'s items should not be empty"
        | x :: rest -> monkeys |> List.updateAt from' sender' |> List.updateAt to' reciever'

    let takeTurn from' adjuster (monkeys: Monkey list) =
        let sender = monkeys[from']
        let receivers, sender' = Monkey.inspectAll adjuster sender
        let monkeys' = List.updateAt from' sender' monkeys
        receivers |> List.fold (fun monkeys to' -> throw from' to' monkeys) monkeys'

    let round adjuster (monkeys: Monkey list) =
        [ 0 .. monkeys.Length - 1 ]
        |> List.fold (fun monkeys from' -> takeTurn from' adjuster monkeys) monkeys

module StringPlus =
    let startsWith (searchString: string) (s: string) =
        searchString = s[.. searchString.Length - 1]

    let remove (target: char) (s: string) = s |> String.filter (fun c -> c <> ' ')

module Parser =
    open System.Text.RegularExpressions

    type Token =
        | Id of MonkeyId
        | Items of WorryLevel list
        | Operation of Operator * Operand
        | PickerCondition of WorryLevel
        | PickerBranch of bool * MonkeyId
        | Divider


    let (|Id|Items|Operation|PickerCondition|PickerBranch|Empty|) (rawLine: string) =
        let structuredLine = rawLine.Split(":") |> Array.map (fun v -> v.Trim())

        match structuredLine with
        | [| key; "" |] ->
            match key.Split(" ") with
            | [| "Monkey"; monkeyId |] -> Id(int monkeyId)
            | _ -> failwith "Could not parse monkey ID"
        | [| "Starting items"; value |] ->
            value.Split ","
            |> Array.map (StringPlus.remove ' ')
            |> Array.map uint64
            |> Array.toList
            |> (fun levels -> Items levels)
        | [| "Operation"; value |] ->
            match value.Split(" ") with
            | [| "new"; "="; "old"; opChar; operand |] ->
                let op =
                    match opChar with
                    | "+" -> Add
                    | "*" -> Multiply
                    | _ -> failwith $"Only support '+' or '*' operator, got {opChar}"

                let operand' =
                    match operand with
                    | "old" -> Variable
                    | _ -> Literal(uint64 operand)

                Operation(op, operand')

            | _ -> failwith "Could not parse Operation"
        | [| "Test"; value |] ->
            let m = Regex.Match(value, @"divisible by (\d+)")
            let brerakpoint = m.Groups[1].Captures[0].Value
            PickerCondition(uint64 brerakpoint)
        | [| "If true"; value |] ->
            let m = Regex.Match(value, @"throw to monkey (\d+)")
            let monkeyId = m.Groups[1].Captures[0].Value
            PickerBranch(true, int monkeyId)
        | [| "If false"; value |] ->
            let m = Regex.Match(value, @"throw to monkey (\d+)")
            let monkeyId = m.Groups[1].Captures[0].Value
            PickerBranch(false, int monkeyId)
        | _ -> Empty

    let parseMonkey (tokens: Token list) =
        let monkeyId =
            match tokens[0] with
            | Token.Id monkeyId -> monkeyId
            | _ -> failwith "An error occurred"

        let items =
            match tokens[1] with
            | Token.Items items -> items
            | _ -> failwith "An error occurred"

        let operation =
            match tokens[2] with
            | Token.Operation(operator, operand) -> (operator, operand)
            | _ -> failwith "An error occurred"

        let pick =
            match (tokens[3], tokens[4], tokens[5]) with
            | ((Token.PickerCondition cond), (Token.PickerBranch(true, th)), (Token.PickerBranch(false, el))) ->
                (cond, th, el)
            | _ -> failwith "An error occurred"

        let monkey =
            { MonkeyId = monkeyId
              Items = items
              Operate = operation
              Pick = pick
              Stats = 0UL }

        monkey

    let groupByMonkey (tokens) =
        seq {
            let mutable rest: Token list = tokens

            while rest.Length > 0 do
                let dividerIndex =
                    tokens
                    |> List.tryFindIndex (function
                        | Token.Divider -> true
                        | _ -> false)

                let monkey, others =
                    match dividerIndex with
                    | None -> rest, []
                    | Some dividerIndex -> List.splitAt dividerIndex rest

                match others with
                | divider :: others -> rest <- others
                | _ -> rest <- others

                yield parseMonkey monkey
        }


    let parseLines lines =
        lines
        |> List.map (function
            | Id id -> Token.Id id
            | Items items -> Token.Items items
            | Operation(operator, operand) -> Token.Operation(operator, operand)
            | PickerCondition level -> Token.PickerCondition level
            | PickerBranch(divisible, monkeyId) -> Token.PickerBranch(divisible, monkeyId)
            | Empty -> Token.Divider)
        |> groupByMonkey

    let read path =
        let lines = System.IO.File.ReadAllLines path
        parseLines (List.ofArray lines)
