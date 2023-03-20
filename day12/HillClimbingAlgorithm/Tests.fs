module Tests

open Xunit
open HillClimbing

open Xunit


[<Fact>]
let ``Breadth-first search`` () =
    let board = array2D [ [ Middle(23u); Middle(24u) ]; [ Middle(23u); End ] ]
    let start = (0, 0)

    let distanceMap, predMap = breathFirstSearch board start

    let expected = array2D [ [ 0; 1 ]; [ 1; 2 ] ]

    Assert.Equal(expected, distanceMap)
