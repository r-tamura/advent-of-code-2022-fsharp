module Tests

open Xunit
open RegolithReservoir

let assertCave (expected: Cave) (actual: Cave) =
    let actualCaveState = actual.cells
    let expectedCaveState = expected.cells
    Assert.Equal(Array2D.length1 expectedCaveState, Array2D.length1 actualCaveState)
    Assert.Equal(Array2D.length2 expectedCaveState, Array2D.length2 actualCaveState)
    Assert.Equal<Cave>(expected, actual)

module Parser =
    [<Fact>]
    let ``parse single line rock path text`` () =
        let input = "1, 1 -> 1, 2"
        let actual = Parser.parse input
        let expected = [ [ (1, 1); (1, 2) ] ]
        Assert.Equal<(int * int) list list>(expected, actual)

    [<Fact>]
    let ``parse multiple rock paths`` () =
        let input =
            "1, 1 -> 1, 2 -> 3, 4
5, 6 -> 7, 8"

        let actual = Parser.parse input
        let expected = [ [ (1, 1); (1, 2); (3, 4) ]; [ (5, 6); (7, 8) ] ]
        Assert.Equal<(int * int) list list>(expected, actual)

    [<Fact>]
    let ``given multiple rock paths with empty lines, should ignore the empty lines`` () =
        let input =
            "1, 1 -> 1, 2 -> 3, 4

5, 6 -> 7, 8
"

        let actual = Parser.parse input
        let expected = [ [ (1, 1); (1, 2); (3, 4) ]; [ (5, 6); (7, 8) ] ]
        Assert.Equal<(int * int) list list>(expected, actual)

module RockPath =
    [<Fact>]
    let ``Given a rock path with a line, should return a tuple of left-top and right-bottom point`` () =

        let path = [ (1, 1); (1, 2) ]
        let actual = RockPath.boundingRect path
        let expected = LocalPosition(1, 1), LocalPosition(1, 2)
        Assert.Equal<LocalPosition * LocalPosition>(expected, actual)


    [<Fact>]
    let ``Given a rock path with multi lines, should return a tuple of left-top and right-bottom point`` () =

        let path = [ (1, 1); (1, 2); (2, 2) ]
        let actual = RockPath.boundingRect path
        let expected = LocalPosition(1, 1), LocalPosition(2, 2)
        Assert.Equal<LocalPosition * LocalPosition>(expected, actual)


module Cave =
    [<Fact>]
    let ``creat a cave`` () =
        //  012345
        // 0.....
        // 1.#...
        // 2.#...
        // 3..##.
        // 4xxxxx
        let rockPaths = [ [ (1, 1); (1, 2) ]; [ (3, 3); (2, 3) ] ]

        let actual = Cave.create rockPaths

        let expectedState =
            array2D
                [ [ Air; Air; Air; Air; Air ]
                  [ Air; Rock; Air; Air; Air ]
                  [ Air; Rock; Air; Air; Air ]
                  [ Air; Air; Rock; Rock; Air ] ]

        let expected = { leftSide = 0; cells = expectedState }

        assertCave expected actual


    [<Fact>]
    let ``create cave (sample from the problem)`` () =
        // ......+...
        // ..........
        // ..........
        // ..........
        // ....#...##
        // ....#...#.
        // ..###...#.
        // ........#.
        // ........#.
        // #########.
        let rockPaths =
            [ [ (498, 4); (498, 6); (496, 6) ]; [ (503, 4); (502, 4); (502, 9); (494, 9) ] ]

        let actual = Cave.create rockPaths

        let mutable expectedState =
            array2D
                [ [ Air; Air; Air; Air; Air; Air; Air; Air; Air; Air; Air; Air ]
                  [ Air; Air; Air; Air; Air; Air; Air; Air; Air; Air; Air; Air ]
                  [ Air; Air; Air; Air; Air; Air; Air; Air; Air; Air; Air; Air ]
                  [ Air; Air; Air; Air; Air; Air; Air; Air; Air; Air; Air; Air ]
                  [ Air; Air; Air; Air; Air; Rock; Air; Air; Air; Rock; Rock; Air ]
                  [ Air; Air; Air; Air; Air; Rock; Air; Air; Air; Rock; Air; Air ]
                  [ Air; Air; Air; Rock; Rock; Rock; Air; Air; Air; Rock; Air; Air ]
                  [ Air; Air; Air; Air; Air; Air; Air; Air; Air; Rock; Air; Air ]
                  [ Air; Air; Air; Air; Air; Air; Air; Air; Air; Rock; Air; Air ]
                  [ Air; Rock; Rock; Rock; Rock; Rock; Rock; Rock; Rock; Rock; Air; Air ] ]


        let expected =
            { leftSide = 493
              cells = expectedState }

        assertCave expected actual

    [<Fact>]
    let ``when there is a rock in the drop path, it stop at a rock`` () =
        let rockPaths =
            [ [ (498, 4); (498, 6); (496, 6) ]; [ (503, 4); (502, 4); (502, 9); (494, 9) ] ]

        let cave = Cave.create rockPaths
        let (nextCave, dropResult) = Cave.drop cave

        let mutable expectedState =
            array2D
                [ [ Air; Air; Air; Air; Air; Air; Air; Air; Air; Air; Air; Air ]
                  [ Air; Air; Air; Air; Air; Air; Air; Air; Air; Air; Air; Air ]
                  [ Air; Air; Air; Air; Air; Air; Air; Air; Air; Air; Air; Air ]
                  [ Air; Air; Air; Air; Air; Air; Air; Air; Air; Air; Air; Air ]
                  [ Air; Air; Air; Air; Air; Rock; Air; Air; Air; Rock; Rock; Air ]
                  [ Air; Air; Air; Air; Air; Rock; Air; Air; Air; Rock; Air; Air ]
                  [ Air; Air; Air; Rock; Rock; Rock; Air; Air; Air; Rock; Air; Air ]
                  [ Air; Air; Air; Air; Air; Air; Air; Air; Air; Rock; Air; Air ]
                  [ Air; Air; Air; Air; Air; Air; Air; Sand; Air; Rock; Air; Air ]
                  [ Air; Rock; Rock; Rock; Rock; Rock; Rock; Rock; Rock; Rock; Air; Air ] ]

        let expected =
            { leftSide = 493
              cells = expectedState }

        Assert.Equal(Cave.Hit(GlobalPosition(500, 8)), dropResult)

        assertCave expected cave


    [<Fact>]
    let ``when there is no rock, it drops into void`` () =
        let rockPaths = [ [ (499, 1); (499, 2) ]; [ (501, 1); (501, 2) ] ]

        let cave = Cave.create rockPaths
        let (nextCave, dropResult) = Cave.drop cave

        let mutable expectedState =
            array2D
                [ [ Air; Air; Air; Air; Air ]
                  [ Air; Rock; Air; Rock; Air ]
                  [ Air; Rock; Air; Rock; Air ] ]

        let expected =
            { leftSide = 498
              cells = expectedState }

        Assert.Equal(Cave.Void, dropResult)

        assertCave expected nextCave


    [<Fact>]
    let ``when there is a rock in the way and left is air, it drops to the left`` () =
        let cave =
            { leftSide = 498
              cells =
                array2D
                    [ [ Air; Air; Air; Air; Air ]
                      [ Air; Air; Rock; Air; Air ]
                      [ Rock; Rock; Rock; Rock; Air ] ] }

        let (nextCave, dropResult) = Cave.drop cave

        let expectedState =
            array2D
                [ [ Air; Air; Air; Air; Air ]
                  [ Air; Sand; Rock; Air; Air ]
                  [ Rock; Rock; Rock; Rock; Air ] ]

        let expected =
            { leftSide = 498
              cells = expectedState }

        Assert.Equal(Cave.Hit(LocalPosition(499, 1)), dropResult)

        assertCave expected nextCave

    [<Fact>]
    let ``when there is a rock in the way and left and right is air, it drops to the right`` () =
        let cave =
            { leftSide = 498
              cells =
                array2D
                    [ [ Air; Air; Air; Air; Air ]
                      [ Air; Rock; Rock; Air; Air ]
                      [ Air; Rock; Rock; Rock; Rock ] ] }

        let (nextCave, dropResult) = Cave.drop cave

        let expectedState =
            array2D
                [ [ Air; Air; Air; Air; Air ]
                  [ Air; Rock; Rock; Sand; Air ]
                  [ Air; Rock; Rock; Rock; Rock ] ]

        let expected =
            { leftSide = 498
              cells = expectedState }

        Assert.Equal(Cave.Hit(LocalPosition(501, 1)), dropResult)

        assertCave expected nextCave

    [<Fact>]
    let ``caveの左端に到達したとき、voidへ落ちる`` () =
        let cave =
            { leftSide = 498
              cells =
                array2D
                    [ [ Air; Air; Air; Air; Air ]
                      [ Air; Air; Rock; Rock; Air ]
                      [ Air; Rock; Rock; Rock; Rock ]
                      [ Air; Rock; Rock; Rock; Rock ] ] }

        let (nextCave, dropResult) = Cave.drop cave

        let expectedState =
            array2D
                [ [ Air; Air; Air; Air; Air ]
                  [ Air; Air; Rock; Rock; Air ]
                  [ Air; Rock; Rock; Rock; Rock ]
                  [ Air; Rock; Rock; Rock; Rock ] ]

        let expected =
            { leftSide = 498
              cells = expectedState }

        Assert.Equal(Cave.Void, dropResult)

        assertCave expected nextCave

    [<Fact>]
    let ``caveの右端に到達したとき、voidへ落ちる`` () =
        let cave =
            { leftSide = 498
              cells =
                array2D
                    [ [ Air; Air; Air; Air; Air ]
                      [ Air; Rock; Rock; Air; Air ]
                      [ Rock; Rock; Rock; Rock; Air ]
                      [ Rock; Rock; Rock; Rock; Air ] ] }

        let (nextCave, dropResult) = Cave.drop cave

        let expectedState =
            array2D
                [ [ Air; Air; Air; Air; Air ]
                  [ Air; Rock; Rock; Air; Air ]
                  [ Rock; Rock; Rock; Rock; Air ]
                  [ Rock; Rock; Rock; Rock; Air ] ]

        let expected =
            { leftSide = 498
              cells = expectedState }

        Assert.Equal(Cave.Void, dropResult)

        assertCave expected nextCave

    [<Fact>]
    let ``when there is no rock in the way, no units of sand comes to rest`` () =
        let mutable cave =
            { leftSide = 498
              cells =
                array2D
                    [ [ Air; Air; Air; Air; Air ]
                      [ Air; Rock; Air; Rock; Air ]
                      [ Air; Rock; Air; Rock; Air ]
                      [ Air; Rock; Air; Rock; Air ] ] }

        let actualCave, sandPositions = Cave.dropUnitsOfSandUntilFlowingIntoVoid cave

        let expected =
            { leftSide = 498
              cells =
                array2D
                    [ [ Air; Air; Air; Air; Air ]
                      [ Air; Rock; Air; Rock; Air ]
                      [ Air; Rock; Air; Rock; Air ]
                      [ Air; Rock; Air; Rock; Air ] ] }

        Assert.Equal<GlobalPosition list>([], sandPositions)
        assertCave expected actualCave

    [<Fact>]
    let ``when there is a rock in the way at the bottom of the flowing point, one unit of sand comes to rest`` () =
        let mutable cave =
            { leftSide = 498
              cells =
                array2D
                    [ [ Air; Air; Air; Air; Air ]
                      [ Air; Air; Air; Air; Air ]
                      [ Air; Rock; Rock; Rock; Air ] ] }

        let actualCave, sandPositions = Cave.dropUnitsOfSandUntilFlowingIntoVoid cave

        let expected =
            { leftSide = 498
              cells =
                array2D
                    [ [ Air; Air; Air; Air; Air ]
                      [ Air; Air; Sand; Air; Air ]
                      [ Air; Rock; Rock; Rock; Air ] ] }

        Assert.Equal<GlobalPosition list>([ GlobalPosition(500, 1) ], sandPositions)
        assertCave expected actualCave
