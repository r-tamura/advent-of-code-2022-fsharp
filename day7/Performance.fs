module Performance

type internal Marker =
    interface
    end

type TimerBuilder() =
    let mutable startTime = 0L

    member this.Delay(f) =
        let delayed =
            fun () ->
                let delayedResult = f ()
                delayedResult

        delayed

    member this.Run(f) =
        startTime <- System.Diagnostics.Stopwatch.GetTimestamp()
        let r = f ()

        (System.Diagnostics.Stopwatch.GetTimestamp() - startTime) * 1000L
        / System.Diagnostics.Stopwatch.Frequency
        |> float
        |> (fun v -> v / 1000.0)
        |> printfn ("[%A] time elapsed: %f sec") typeof<Marker>.DeclaringType

        r

    member this.Zero() = ()

let timer = TimerBuilder()
