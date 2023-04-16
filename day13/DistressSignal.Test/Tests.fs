module Tests

open System
open Xunit

open DistressSignal
open FParsec

let test input =
    match Parser.parse input with
    | Success (result, _, _) -> result
    | Failure (message, _, _) -> failwith message

module Parser =

    [<Fact>]
    let ``Parse a pair of packets`` () =
        let input = """[1, 2, 3]
                    [4, 5, 6]
                    """
        let actual = test input
        let expected = [(
            List [Number 1; Number 2; Number 3],
            List [Number 4; Number 5; Number 6]
        )]
        Assert.Equal<(Packet * Packet) list>(expected, actual)

    [<Fact>]
    let ``Parse multiple pairs of packets`` () =
        let input = """[1, 2, 3]
                    [4, 5, 6]

                    [[7]]
                    [[]]
                    """
        let actual = test input
        let expected = [(
            List [Number 1; Number 2; Number 3],
            List [Number 4; Number 5; Number 6]
        ); (List [List [Number 7]], List [List []])]
        Assert.Equal<(Packet * Packet) list>(expected, actual)

module Solution =
    open Packet

    let COMPARE_NUMBERS_PARAMS: obj[] list =
        [ [|1; 2; RightOrder|]
          [|2; 1; WrongOrder|]
          [|1; 1; Same|] ]

    [<Theory>]
    [<MemberData(nameof(COMPARE_NUMBERS_PARAMS))>]
    let ``Compare two numbers`` (a, b, expected) =
        let actual = Packet.compareNumbers a b
        Assert.Equal<CompareResult>(expected, actual)

    let EMPTY: Packet list = []
    let COMPRAE_PACKETS_PARAMS: obj[] list = [
        [| [Number 1]; [Number 2]; RightOrder |]
        [| [Number 2]; [Number 1]; WrongOrder |]
        [| [Number 1; Number 2; Number 3]; [Number 1; Number 2; Number 3]; Same |]
        [| EMPTY; [Number 1]; RightOrder |]
        [| [Number 1]; EMPTY; WrongOrder |]
        [| [List [Number 1]; List [Number 2; Number 3; Number 4]]; [List [Number 1]; List [Number 4]]; RightOrder|]
        [| [List [List[List[]]]]; [List [ List []]]; WrongOrder|]
    ]

    [<Theory>]
    [<MemberData(nameof(COMPRAE_PACKETS_PARAMS))>]
    let ``Compare two lists`` (a, b, expected) =
        let actual = Packet.compareLists a b
        Assert.Equal<CompareResult>(expected, actual)


    let COMPARE_PAIR_PARAMS: obj[] list = [
        [| List [Number 9]; List [ List [Number 8; Number 7; Number 6]]; WrongOrder |]
        [| List [List [Number 8; Number 7; Number 6]]; List [Number 9]; RightOrder |]
    ]

    [<Theory>]
    [<MemberData(nameof(COMPARE_PAIR_PARAMS))>]
    let ``Compare two pair`` (a, b, expected) =
        let actual = Packet.comparePair (a, b)
        Assert.Equal<CompareResult>(expected, actual)