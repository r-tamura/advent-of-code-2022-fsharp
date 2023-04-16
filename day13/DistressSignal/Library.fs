namespace DistressSignal

open System
open FParsec

type Packet =
    | Number of int
    | List of Packet list

type PacketPair = Packet * Packet

module Parser =
    let pnumber =
        pint32 |>> fun i -> Number i

    let pvalue, pvalueRef = createParserForwardedToRef<Packet, unit>()

    let ppacket =
        let left = pchar '[' .>> spaces
        let right = pchar ']'
        let value = spaces >>. pvalue .>> spaces
        let comma = pchar ',' .>> spaces
        let values = (sepBy value comma)
        let whitespaces = many (pchar ' ')

        whitespaces >>. between left right values .>> whitespaces
        |>> fun values -> List values

    let ppairOfPackets = ppacket .>> newline .>> spaces .>>. ppacket .>> opt newline

    let ppairs =
        sepBy ppairOfPackets newline

    let parse input =
        let result = run ppairs input
        match result with
        | Success (packets, _, _) -> packets
        | _ -> failwith "Could not parse input."

    pvalueRef.Value <- choice [pnumber; ppacket]


module Packet =
    // Rule
    // - If both values are integers, the lower integer should come first. If the left integer is lower than the right integer, the inputs are in the right order. If the left integer is higher than the right integer, the inputs are not in the right order. Otherwise, the inputs are the same integer; continue checking the next part of the input.
    // - If both values are lists, compare the first value of each list, then the second value, and so on. If the left list runs out of items first, the inputs are in the right order. If the right list runs out of items first, the inputs are not in the right order. If the lists are the same length and no comparison makes a decision about the order, continue checking the next part of the input.
    // - If exactly one value is an integer, convert the integer to a list which contains that integer as its only value, then retry the comparison. For example, if comparing [0,0,0] and 2, convert the right value to [2] (a list containing 2); the result is then found by instead comparing [0,0,0] and [2].
    type CompareResult =
        | RightOrder
        | WrongOrder
        | Same

    let compareNumbers (left: int) (right: int) =
        if left < right then RightOrder
        elif left > right then WrongOrder
        else Same

    let rec compareLists (left: Packet list) (right: Packet list) =
        let rec compare (left: Packet list) (right: Packet list) =
            match left, right with
            | [], [] -> Same
            | [], _ -> RightOrder // If the left list runs out of items first, the inputs are in the right order
            | _, [] -> WrongOrder // If the right list runs out of items first, the inputs are not in the right order
            | left :: xs, right :: ys ->
                let result = comparePair (left, right)
                match result with
                | Same -> compare xs ys
                | _ -> result

        compare left right
    and comparePair (pair: PacketPair) =
        match pair with
        | Number a, Number b -> compareNumbers a b
        | List a, List b -> compareLists a b
        | Number n, List l ->
            compareLists [Number n]  l
        | List l, Number n ->
            compareLists l [Number n]
