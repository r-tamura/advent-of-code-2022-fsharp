namespace CathodeRayTube

open System.Collections.Generic

type Instruction =
    | Addx of int
    | Noop

module Instruction =
    let parse (s: string) =
        match s.Split(" ") with
        | [| "noop" |] -> Noop
        | [| "addx"; operand1 |] -> Addx(int operand1)
        | _ -> failwith $"invalid operation: {s}"


type VM =
    { Cycle: uint
      X: int
      Queue: Queue<uint * Instruction> }


module VM =
    let init =
        { Cycle = 1u
          X = 1
          Queue = new Queue<uint * Instruction>() }


    let private enqueue (vm: VM) instr =
        match instr with
        | Noop -> vm.Queue.Enqueue((vm.Cycle, Noop))
        | Addx v -> vm.Queue.Enqueue((vm.Cycle + 1u, Addx v))

        vm.Queue

    let private execute instr vm =
        let nextX =
            match instr with
            | Addx v -> vm.X + v
            | _ -> vm.X

        nextX

    let private tick vm instr =

        let nextQueue =
            match vm.Queue.Count with
            | 0 -> enqueue vm instr
            | _ -> vm.Queue

        let nextX, nextQueue2 =
            match vm.Queue.Peek() with
            | (cycle, instr) when cycle = vm.Cycle ->
                vm.Queue.Dequeue() |> ignore
                (execute instr vm, vm.Queue)
            | _ -> (vm.X, nextQueue)

        let nextCycle = vm.Cycle + 1u

        { Cycle = nextCycle
          X = nextX
          Queue = nextQueue2 }

    let toSeq (instructions: Instruction seq) (vm: VM) =
        seq {
            let mutable _vm = vm

            for instr in instructions do
                _vm <- tick _vm instr
                yield _vm

                while _vm.Queue.Count > 0 do
                    _vm <- tick _vm instr
                    yield _vm
        }

    let run (instructions: Instruction seq) (vm: VM) =
        (instructions, vm) ||> toSeq |> Seq.reduce (fun acc vm -> vm)
