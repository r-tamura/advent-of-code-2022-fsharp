module Tests

open Xunit
open RopeBridge

module Motion =
    [<Fact>]
    let ``Should Parse Motion Line`` () =
        let actual = Motion.parse "R 5"
        Assert.Equal(Motion.Right 5, actual)


module State =
    [<Fact>]
    let ``init`` () =
        let actual = State.init

        let expected =
            { Head = (0, 0)
              Tail = (0, 0)
              Footprints = Set([ (0, 0) ]) }

        Assert.Equal(expected, actual)

    // [<Fact>]
    // let ``toString`` () =
    //     let state =
    //         { Head = (0, 0)
    //           Tail = (0, 0)
    //           Board = array2D [ [ true; false ]; [ false; true ] ] }

    //     let actual = State.toString state

    //     Assert.Equal(
    //         "#.\n\
    //          .#",
    //         actual
    //     )

    [<Fact>]
    let ``Count up positions the tail visited`` () =
        let state =
            { Head = (0, 0)
              Tail = (0, 0)
              Footprints = Set([ (0, 0) ]) }

        let actual = State.countVisited state
        Assert.Equal(1, actual)


    module Step =
        let parameters: obj[] list =
            [ [| "When the head and the tail are overwrapped, If the head moves, the tail should not move"
                 [| (Motion.Right 1) |]
                 { Head = (0, 1)
                   Tail = (0, 0)
                   Footprints = Set([ (0, 0) ]) } |]
              [| "When the head and the tail are on the same row but not overlapping, if the head moves horizontally, the tail should move in the same direction."
                 [| (Motion.Right 1); (Motion.Right 1) |]
                 { Head = (0, 2)
                   Tail = (0, 1)
                   Footprints = Set([ (0, 0); (0, 1) ]) } |]
              [| "When the head and the tail are on the same row but not overlapping, if the head moves vertically, the tail should move in the same direction."
                 [| (Motion.Up 2) |]
                 { Head = (-2, 0)
                   Tail = (-1, 0)
                   Footprints = Set([ (0, 0); (-1, 0) ]) } |]
              [| "If the head and the tail are diagonally adjacent, the tail should not move"
                 [| (Motion.Up 1); (Motion.Right 1) |]
                 { Head = (-1, 1)
                   Tail = (0, 0)
                   Footprints = Set([ (0, 0) ]) } |]
              [| "When the head and tail aren't touching and aren't in the same row or column, the tail always moves one step diagonally to keep up"
                 [| (Motion.Up 1); (Motion.Right 2) |]
                 { Head = (-1, 2)
                   Tail = (-1, 1)
                   Footprints = Set([ (0, 0); (-1, 1) ]) } |] ]

        [<Theory>]
        [<MemberData(nameof (parameters))>]
        let ``Step`` (title: string) (motions: Motion[]) (expected: State) =
            let initialState = State.init

            let actual =
                motions |> Array.fold (fun acc motion -> State.step motion acc) initialState

            Assert.Equal(expected.Head, actual.Head)
            Assert.Equal(expected.Tail, actual.Tail)
            Assert.Equal<Set<Position>>(expected.Footprints, actual.Footprints)
