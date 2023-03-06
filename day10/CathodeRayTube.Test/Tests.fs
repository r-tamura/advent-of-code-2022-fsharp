module Tests

open System.Collections.Generic

open Xunit
open CathodeRayTube

[<Fact>]
let ``Single Noop`` () =
    let instrs = [| Noop |]

    let vm = VM.init
    let actual = VM.run instrs vm

    let expected =
        { Cycle = 2u
          X = 1
          Queue = new Queue<uint * Instruction>() }

    Assert.Equal(expected.Cycle, actual.Cycle)
    Assert.Equal(expected.X, actual.X)


[<Fact>]
let ``Multi Noop`` () =
    let instrs = [| Noop; Noop |]

    let vm = VM.init
    let actual = VM.run instrs vm

    let expected =
        { Cycle = 3u
          X = 1
          Queue = new Queue<uint * Instruction>() }

    Assert.Equal(expected.Cycle, actual.Cycle)
    Assert.Equal(expected.X, actual.X)

[<Fact>]
let ``Addx`` () =
    let instrs = [| Addx 1 |]

    let vm = VM.init
    let actual = VM.run instrs vm

    let expected =
        { Cycle = 3u
          X = 2
          Queue = new Queue<uint * Instruction>() }

    Assert.Equal(expected.Cycle, actual.Cycle)
    Assert.Equal(expected.X, actual.X)
