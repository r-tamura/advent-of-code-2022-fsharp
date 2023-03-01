module Tests

open System
open Xunit

open TreetopTreeHouse


module Part1 =
    [<Fact>]
    let ``Count trees visble from outside of the grid`` () =
        let input =
            array2D
                [ [ 3; 0; 3; 7; 3 ]
                  [ 2; 5; 5; 1; 2 ]
                  [ 6; 5; 3; 3; 2 ]
                  [ 3; 3; 5; 4; 9 ]
                  [ 3; 5; 3; 9; 0 ] ]

        let actual = TreetopTreeHouse.countVisible input

        Assert.Equal(21, actual)

    [<Fact>]
    let ``Should be visible all`` () =
        let input = array2D [ [ 3; 0 ]; [ 2; 5 ] ]

        let actual = TreetopTreeHouse.countVisible input

        Assert.Equal(4, actual)

    [<Fact>]
    let ``Should not be visible other than edge ones`` () =
        let input =
            array2D
                [ [ 9; 9; 9; 9; 9 ]
                  [ 9; 9; 9; 9; 9 ]
                  [ 9; 9; 9; 9; 9 ]
                  [ 9; 9; 9; 9; 9 ]
                  [ 9; 9; 9; 9; 9 ] ]

        let actual = TreetopTreeHouse.countVisible input

        Assert.Equal(16, actual)


module Part2 =
    [<Fact>]
    let ``Find Highest Scenic Score`` () =
        let input =
            array2D
                [ [ 3; 0; 3; 7; 3 ]
                  [ 2; 5; 5; 1; 2 ]
                  [ 6; 5; 3; 3; 2 ]
                  [ 3; 3; 5; 4; 9 ]
                  [ 3; 5; 3; 9; 0 ] ]

        let actual = TreetopTreeHouse.findHighestSenicScore input

        Assert.Equal(8, actual)
