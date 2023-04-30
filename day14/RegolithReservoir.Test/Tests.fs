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
    let ``create a cave with void`` () =
        //            1
        //  01234567890
        // 0...........
        // 1....#......
        // 2....#......
        // 3.....##....
        // 4...........
        // 6...........
        let rockPaths = [ [ (499, 1); (499, 2) ]; [ (500, 3); (501, 3) ] ]

        let actual = Cave.create Cave.EndlessVoid rockPaths

        let expectedState =
            array2D
                [ [ A; A; A; A; A; A; A; A; A ]
                  [ A; A; A; R; A; A; A; A; A ]
                  [ A; A; A; R; A; A; A; A; A ]
                  [ A; A; A; A; R; R; A; A; A ] ]

        let expected =
            { leftSide = 496
              cells = expectedState }

        assertCave expected actual

    [<Fact>]
    let ``create a cave with the infinite horizontal line`` () =
        //            1
        //  01234567890
        // 0...........
        // 1....#......
        // 2....#......
        // 3.....##....
        // 4...........
        // 6###########
        let rockPaths = [ [ (499, 1); (499, 2) ]; [ (500, 3); (501, 3) ] ]

        let actual = Cave.create (Cave.InfiniteHorizontalLine 2) rockPaths

        let expectedState =
            array2D
                [ [ A; A; A; A; A; A; A; A; A; A; A; A; A ]
                  [ A; A; A; A; A; R; A; A; A; A; A; A; A ]
                  [ A; A; A; A; A; R; A; A; A; A; A; A; A ]
                  [ A; A; A; A; A; A; R; R; A; A; A; A; A ]
                  [ A; A; A; A; A; A; A; A; A; A; A; A; A ]
                  [ R; R; R; R; R; R; R; R; R; R; R; R; R ] ]

        let expected =
            { leftSide = 494
              cells = expectedState }

        assertCave expected actual


    [<Fact>]
    let ``when there is no rock, it drops into void`` () =
        let cave =
            { leftSide = 495
              cells =
                array2D
                    [ [ A; A; A; A; A; A; A; A; A; A; A ]
                      [ A; A; A; A; R; A; R; A; A; A; A ]
                      [ A; A; A; A; R; A; R; A; A; A; A ] ] }

        let (nextCave, dropResult) = Cave.drop cave

        let mutable expectedState =
            array2D
                [ [ A; A; A; A; A; A; A; A; A; A; A ]
                  [ A; A; A; A; R; A; R; A; A; A; A ]
                  [ A; A; A; A; R; A; R; A; A; A; A ] ]

        let expected =
            { leftSide = 495
              cells = expectedState }

        Assert.Equal(Cave.Void, dropResult)

        assertCave expected nextCave


    [<Fact>]
    let ``when there is a rock in the way and left is air, it drops to the left`` () =
        let cave =
            { leftSide = 498
              cells = array2D [ [ A; A; A; A; A ]; [ A; A; R; A; A ]; [ R; R; R; R; A ] ] }

        let (nextCave, dropResult) = Cave.drop cave

        let expectedState =
            array2D [ [ A; A; A; A; A ]; [ A; S; R; A; A ]; [ R; R; R; R; A ] ]

        let expected =
            { leftSide = 498
              cells = expectedState }

        Assert.Equal(Cave.Hit(LocalPosition(499, 1)), dropResult)

        assertCave expected nextCave

    [<Fact>]
    let ``when there is a rock in the way and left and right is air, it drops to the right`` () =
        let cave =
            { leftSide = 498
              cells = array2D [ [ A; A; A; A; A ]; [ A; R; R; A; A ]; [ A; R; R; R; R ] ] }

        let (nextCave, dropResult) = Cave.drop cave

        let expectedState =
            array2D [ [ A; A; A; A; A ]; [ A; R; R; S; A ]; [ A; R; R; R; R ] ]

        let expected =
            { leftSide = 498
              cells = expectedState }

        Assert.Equal(Cave.Hit(LocalPosition(501, 1)), dropResult)

        assertCave expected nextCave

    [<Fact>]
    let ``caveの左端に到達したとき、voidへ落ちる`` () =
        let cave =
            { leftSide = 498
              cells = array2D [ [ A; A; A; A; A ]; [ A; A; R; R; A ]; [ A; R; R; R; R ]; [ A; R; R; R; R ] ] }

        let (nextCave, dropResult) = Cave.drop cave

        let expectedState =
            array2D [ [ A; A; A; A; A ]; [ A; A; R; R; A ]; [ A; R; R; R; R ]; [ A; R; R; R; R ] ]

        let expected =
            { leftSide = 498
              cells = expectedState }

        Assert.Equal(Cave.Void, dropResult)

        assertCave expected nextCave

    [<Fact>]
    let ``caveの右端に到達したとき、voidへ落ちる`` () =
        let cave =
            { leftSide = 498
              cells = array2D [ [ A; A; A; A; A ]; [ A; R; R; A; A ]; [ R; R; R; R; A ]; [ R; R; R; R; A ] ] }

        let (nextCave, dropResult) = Cave.drop cave

        let expectedState =
            array2D [ [ A; A; A; A; A ]; [ A; R; R; A; A ]; [ R; R; R; R; A ]; [ R; R; R; R; A ] ]

        let expected =
            { leftSide = 498
              cells = expectedState }

        Assert.Equal(Cave.Void, dropResult)

        assertCave expected nextCave

    [<Fact>]
    let ``when there is no rock in the way, no units of sand comes to rest`` () =
        let cave =
            { leftSide = 498
              cells = array2D [ [ A; A; A; A; A ]; [ A; R; A; R; A ]; [ A; R; A; R; A ]; [ A; R; A; R; A ] ] }

        let actualCave, sandPositions = Cave.dropUnitsOfSandUntilFlowingIntoVoid cave

        let expected =
            { leftSide = 498
              cells = array2D [ [ A; A; A; A; A ]; [ A; R; A; R; A ]; [ A; R; A; R; A ]; [ A; R; A; R; A ] ] }

        Assert.Equal<GlobalPosition list>([], sandPositions)
        assertCave expected actualCave

    [<Fact>]
    let ``when there is a rock in the way at the bottom of the flowing point, one unit of sand comes to rest`` () =
        let cave =
            { leftSide = 498
              cells = array2D [ [ A; A; A; A; A ]; [ A; A; A; A; A ]; [ A; R; R; R; A ] ] }

        let actualCave, sandPositions = Cave.dropUnitsOfSandUntilFlowingIntoVoid cave

        let expected =
            { leftSide = 498
              cells = array2D [ [ A; A; A; A; A ]; [ A; A; S; A; A ]; [ A; R; R; R; A ] ] }

        Assert.Equal<GlobalPosition list>([ GlobalPosition(500, 1) ], sandPositions)
        assertCave expected actualCave

    [<Fact>]
    let ``when 2 x 6`` () =
        //  0123456
        // 0...x...
        // 1..xxx..
        // 2.xxxxx.
        // 3#######
        let cave =
            { leftSide = 497
              cells =
                array2D
                    [ [ A; A; A; A; A; A; A ]
                      [ A; A; A; A; A; A; A ]
                      [ A; A; A; A; A; A; A ]
                      [ R; R; R; R; R; R; R ] ] }

        let actualCave =
            Cave.dropUnitsOfSandUntilAUnitOfSandComesToRestAtTheFlowingPoint cave

        let expected =
            { leftSide = 497
              cells =
                array2D
                    [ [ A; A; A; S; A; A; A ]
                      [ A; A; S; S; S; A; A ]
                      [ A; S; S; S; S; S; A ]
                      [ R; R; R; R; R; R; R ] ] }

        assertCave expected actualCave


    [<Fact>]
    let ``part2 sample`` () =
        // ............o............
        // ...........ooo...........
        // ..........ooooo..........
        // .........ooooooo.........
        // ........oo#ooo##o........
        // .......ooo#ooo#ooo.......
        // ......oo###ooo#oooo......
        // .....oooo.oooo#ooooo.....
        // ....oooooooooo#oooooo....
        // ...ooo#########ooooooo...
        // ..ooooo.......ooooooooo..
        // #########################
        let rockPaths =
            [ [ (498, 4); (498, 6); (496, 6) ]; [ (503, 4); (502, 4); (502, 9); (494, 9) ] ]

        let cave = Cave.create (Cave.InfiniteHorizontalLine 2) rockPaths
        let actual = Cave.dropUnitsOfSandUntilAUnitOfSandComesToRestAtTheFlowingPoint cave

        let expectedCells =
            "............o............
...........ooo...........
..........ooooo..........
.........ooooooo.........
........oo#ooo##o........
.......ooo#ooo#ooo.......
......oo###ooo#oooo......
.....oooo.oooo#ooooo.....
....oooooooooo#oooooo....
...ooo#########ooooooo...
..ooooo.......ooooooooo..
#########################"


        Assert.Equal(expectedCells, (Cave.toStr actual))
