module Tests

open Xunit
open RopeBridge

module Motion =
    [<Fact>]
    let ``Should Parse Motion Line`` () =
        let actual = Motion.parse "R 5"
        Assert.Equal(Motion.Right 5, actual)


module State =
    module TwoPoints =
        [<Fact>]
        let ``init`` () =
            let actual = State.init [| (0, 0); (0, 0) |]

            let expected =
                { Knots = [| (0, 0); (0, 0) |]
                  Footprints = Set([ (0, 0) ]) }

            Assert.Equal(expected, actual)

        [<Fact>]
        let ``toString`` () =
            let state = State.init (Array.create 3 (0, 0))

            let actual = State.toString state

            Assert.Equal(
                "...\n\
                 ...\n\
                 H..",
                actual
            )

        [<Fact>]
        let ``Count up positions the tail visited`` () =
            let state =
                { Knots = [| (0, 0); (0, 0) |]
                  Footprints = Set([ (0, 0) ]) }

            let actual = State.countVisited state
            Assert.Equal(1, actual)


        module Step =
            let parameters: obj[] list =
                [ [| "When the head and the tail are overwrapped, If the head moves, the tail should not move"
                     [| (Motion.Right 1) |]
                     { Knots = [| (0, 1); (0, 0) |]
                       Footprints = Set([ (0, 0) ]) } |]
                  [| "When the head and the tail are on the same row but not overlapping, if the head moves horizontally, the tail should move in the same direction."
                     [| (Motion.Right 1); (Motion.Right 1) |]
                     { Knots = [| (0, 2); (0, 1) |]
                       Footprints = Set([ (0, 0); (0, 1) ]) } |]
                  [| "When the head and the tail are on the same row but not overlapping, if the head moves vertically, the tail should move in the same direction."
                     [| (Motion.Up 2) |]
                     { Knots = [| (-2, 0); (-1, 0) |]
                       Footprints = Set([ (0, 0); (-1, 0) ]) } |]
                  [| "If the head and the tail are diagonally adjacent, the tail should not move"
                     [| (Motion.Up 1); (Motion.Right 1) |]
                     { Knots = [| (-1, 1); (0, 0) |]
                       Footprints = Set([ (0, 0) ]) } |]
                  [| "When the head and tail aren't touching and aren't in the same row or column, the tail always moves one step diagonally to keep up"
                     [| (Motion.Up 1); (Motion.Right 2) |]
                     { Knots = [| (-1, 2); (-1, 1) |]
                       Footprints = Set([ (0, 0); (-1, 1) ]) } |] ]

            [<Theory>]
            [<MemberData(nameof (parameters))>]
            let ``Step`` (title: string) (motions: Motion[]) (expected: State) =
                let initialState = State.init [| (0, 0); (0, 0) |]

                let actual =
                    motions |> Array.fold (fun acc motion -> State.step motion acc) initialState

                Assert.Equal<Position[]>(expected.Knots, actual.Knots)
                Assert.Equal<Set<Position>>(expected.Footprints, actual.Footprints)


    module TenPoints =
        [<Fact>]
        let ``Count up positions the tail visited`` () =
            let state =
                { Knots = Array.create 10 (0, 0)
                  Footprints = Set([ (0, 0) ]) }

            let actual = State.countVisited state
            Assert.Equal(1, actual)

        module Step =
            let parameters: obj[] list =
                [ [| "When the head and the tail are overwrapped, If the head moves, the tail should not move"
                     [| (Motion.Right 1) |]
                     { Knots = Array.concat [ [| (0, 1) |]; Array.create 9 (0, 0) ]
                       Footprints = Set([ (0, 0) ]) } |]
                  [| "When the head moves horizontally knots count times, every knot following the head moves"
                     [| (Motion.Right 10) |]
                     { Knots =
                         [| (0, 10)
                            (0, 9)
                            (0, 8)
                            (0, 7)
                            (0, 6)
                            (0, 5)
                            (0, 4)
                            (0, 3)
                            (0, 2)
                            (0, 1) |]
                       Footprints = Set([ (0, 0); (0, 1) ]) } |]
                  [| "先頭の結び目がtouching状態で亡くなったとき、2番目の結び目は先頭の結び目に追従する"
                     [| (Motion.Right 5); (Motion.Up 8) |]
                     { Knots =
                         [| (-8, 5)
                            (-7, 5)
                            (-6, 5)
                            (-5, 5)
                            (-4, 5)
                            (-4, 4)
                            (-3, 3)
                            (-2, 2)
                            (-1, 1)
                            (0, 0) |]
                       Footprints = Set([ (0, 0) ]) } |] ]

            [<Theory>]
            [<MemberData(nameof (parameters))>]
            let ``Step`` (title: string) (motions: Motion[]) (expected: State) =
                let initialState = State.init (Array.create 10 (0, 0))

                let actual =
                    motions |> Array.fold (fun acc motion -> State.step motion acc) initialState

                Assert.Equal<Position[]>(expected.Knots[5..], actual.Knots[5..])
                Assert.Equal<Position[]>(expected.Knots, actual.Knots)
                Assert.Equal<Set<Position>>(expected.Footprints, actual.Footprints)
