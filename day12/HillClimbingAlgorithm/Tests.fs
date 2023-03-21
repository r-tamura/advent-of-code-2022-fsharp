module Tests

open Xunit
open HillClimbing

open Xunit


[<Fact>]
let ``Breadth-first search`` () =
    let board = array2D [ [ Middle(23u); Middle(24u) ]; [ Middle(23u); End ] ]
    let start = (0, 0)

    let distanceMap, predMap = breathFirstSearch HeightMap.neighborsToSummit board start

    let expected = array2D [ [ 0; 1 ]; [ 1; 2 ] ]

    Assert.Equal(expected, distanceMap)


[<Fact>]
let ``find shortest path from the summit to the nearest 'a'`` () =
    let input =
        """Sabqponm
abcryxxl
accszExk
acctuvwj
abdefghi
"""

    let split (s: string) =
        let cells =
            Array.ofSeq (s.Split("\n", System.StringSplitOptions.RemoveEmptyEntries))

        cells

    let board = input |> split |> List.ofArray |> Parser.parse

    let actual = fewestStepCounFromEnd board

    Assert.Equal(29, actual)
