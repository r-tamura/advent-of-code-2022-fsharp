module Tests

open System
open Xunit
open MonkeyInTheMiddle

[<Trait("Size", "Small")>]
module TestStringPlus =
    [<Fact>]
    let ``Test s`` () =
        let x = 42
        Assert.Equal(true, StringPlus.startsWith "Monkey" "Monkey 0:")


[<Trait("Size", "Small")>]
module Parser =
    open MonkeyInTheMiddle.Parser

    [<Fact>]
    let ``tokenize single monkey text format`` () =
        let input =
            [ "Monkey 0:"
              "    Starting items: 79, 98"
              "    Operation: new = old * 19"
              "    Test: divisible by 23"
              "        If true: throw to monkey 2"
              "        If false: throw to monkey 3" ]

        let actual = parseLines input

        let expected =
            [ { MonkeyId = 0
                Items = [ 79; 98 ]
                Operate = (Multiply, Literal(19))
                Pick = (23, 2, 3)
                Stats = 0 } ]


        Assert.Equal<Monkey list>(expected, Seq.toList actual)

    [<Fact>]
    let ``tokenize multiple monkey text format`` () =
        let input =
            [ "Monkey 0:"
              "    Starting items: 79, 98"
              "    Operation: new = old * 19"
              "    Test: divisible by 23"
              "        If true: throw to monkey 2"
              "        If false: throw to monkey 3"
              ""
              "Monkey 1:"
              "    Starting items: 54, 65, 75, 74"
              "    Operation: new = old + old"
              "    Test: divisible by 19"
              "        If true: throw to monkey 2"
              "        If false: throw to monkey 0" ]


        let actual = parseLines input

        let expected =
            [ { MonkeyId = 0
                Items = [ 79; 98 ]
                Operate = (Multiply, Literal(19))
                Pick = (23, 2, 3)
                Stats = 0 }
              { MonkeyId = 1
                Items = [ 54; 65; 75; 74 ]
                Operate = (Add, Variable)
                Pick = (19, 2, 0)
                Stats = 0 } ]

        Assert.Equal<Monkey list>(expected, Seq.toList actual)

    [<Fact>]
    let ``parse single monkey`` () =
        let input =
            [ Token.Id(0)
              Token.Items([ 79; 98 ])
              Token.Operation(Multiply, Literal(19))
              Token.PickerCondition(23)
              Token.PickerBranch(true, 2)
              Token.PickerBranch(false, 3) ]

        let actual = Parser.parseMonkey input

        Assert.Equal(0, actual.MonkeyId)
        Assert.Equal<WorryLevel list>([ 79; 98 ], actual.Items)
        Assert.Equal((Multiply, Literal(19)), actual.Operate)
        Assert.Equal((23, 2, 3), actual.Pick)


[<Trait("Size", "Small")>]
module Monkey =
    [<Fact>]
    let ``Inspect should update the monkey state`` () =
        let initialMonkey =
            { MonkeyId = 0
              Items = [ 10; 20; 30 ]
              Operate = (Multiply, Literal(15))
              Pick = (75, 2, 3)
              Stats = 0 }

        let receivers, monkey = Monkey.inspectAll initialMonkey

        let expectedReceivers = [ 3; 3; 2 ]

        let expectedMonkey =
            { MonkeyId = 0
              Items = [ 50; 100; 150 ]
              Operate = (Multiply, Literal(15))
              Pick = (75, 2, 3)
              Stats = 0 }

        Assert.Equal(expectedMonkey, monkey)
        Assert.Equal<int list>(expectedReceivers, receivers)

[<Trait("Size", "Small")>]
module MonkeyCollection =
    [<Fact>]
    let ``throw a monkey's first item to another monkey `` () =

        let input =
            [ { MonkeyId = 0
                Items = [ 79; 98 ]
                Operate = (Multiply, Literal(19))
                Pick = (23, 2, 3)
                Stats = 0 }
              { MonkeyId = 1
                Items = [ 54; 65; 75; 74 ]
                Operate = (Add, Variable)
                Pick = (19, 2, 0)
                Stats = 0 } ]

        let actual = MonkeyCollection.throw 0 1 input

        let expected =
            [ { MonkeyId = 0
                Items = [ 98 ]
                Operate = (Multiply, Literal(19))
                Pick = (23, 2, 3)
                Stats = 0 }
              { MonkeyId = 1
                Items = [ 54; 65; 75; 74; 79 ]
                Operate = (Add, Variable)
                Pick = (19, 2, 0)
                Stats = 0 } ]

        Assert.Equal<Monkey list>(expected, actual)

    [<Fact>]
    let ``tating a monkey's turn, the monkey throw all of its items`` () =
        let input =
            [ { MonkeyId = 0
                Items = [ 79; 98 ]
                Operate = (Multiply, Literal(19))
                Pick = (23, 2, 3)
                Stats = 0 }
              { MonkeyId = 1
                Items = [ 54; 65; 75; 74 ]
                Operate = (Add, Literal(6))
                Pick = (19, 2, 0)
                Stats = 0 }
              { MonkeyId = 2
                Items = [ 79; 60; 97 ]
                Operate = (Multiply, Variable)
                Pick = (13, 1, 3)
                Stats = 0 }
              { MonkeyId = 3
                Items = [ 74 ]
                Operate = (Add, Literal(3))
                Pick = (17, 0, 1)
                Stats = 0 } ]

        let actual = MonkeyCollection.takeTurn 0 input

        let expected =
            [ { MonkeyId = 0
                Items = []
                Operate = (Multiply, Literal(19))
                Pick = (23, 2, 3)
                Stats = 0 }
              { MonkeyId = 1
                Items = [ 54; 65; 75; 74 ]
                Operate = (Add, Literal(6))
                Pick = (19, 2, 0)
                Stats = 0 }
              { MonkeyId = 2
                Items = [ 79; 60; 97 ]
                Operate = (Multiply, Variable)
                Pick = (13, 1, 3)
                Stats = 0 }
              { MonkeyId = 3
                Items = [ 74; 500; 620 ]
                Operate = (Add, Literal(3))
                Pick = (17, 0, 1)
                Stats = 0 } ]

        Assert.Equal<Monkey list>(expected, actual)

    [<Fact>]
    let ``round`` () =
        let input =
            [ { MonkeyId = 0
                Items = [ 79; 98 ]
                Operate = (Multiply, Literal(19))
                Pick = (23, 2, 3)
                Stats = 0 }
              { MonkeyId = 1
                Items = [ 54; 65; 75; 74 ]
                Operate = (Add, Literal(6))
                Pick = (19, 2, 0)
                Stats = 0 }
              { MonkeyId = 2
                Items = [ 79; 60; 97 ]
                Operate = (Multiply, Variable)
                Pick = (13, 1, 3)
                Stats = 0 }
              { MonkeyId = 3
                Items = [ 74 ]
                Operate = (Add, Literal(3))
                Pick = (17, 0, 1)
                Stats = 0 } ]

        let actual = MonkeyCollection.round input

        let expected =
            [ { MonkeyId = 0
                Items = [ 20; 23; 27; 26 ]
                Operate = (Multiply, Literal(19))
                Pick = (23, 2, 3)
                Stats = 0 }
              { MonkeyId = 1
                Items = [ 2080; 25; 167; 207; 401; 1046 ]
                Operate = (Add, Literal(6))
                Pick = (19, 2, 0)
                Stats = 0 }
              { MonkeyId = 2
                Items = []
                Operate = (Multiply, Variable)
                Pick = (13, 1, 3)
                Stats = 0 }
              { MonkeyId = 3
                Items = []
                Operate = (Add, Literal(3))
                Pick = (17, 0, 1)
                Stats = 0 } ]

        Assert.Equal<Monkey list>(expected, actual)
