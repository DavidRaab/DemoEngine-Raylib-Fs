namespace Benchmark

[<AutoOpen>]
module Whatever =
    let inline benchmark ([<InlineIfLambda>] f) =
        let sw = System.Diagnostics.Stopwatch.StartNew ()
        let value = f ()
        sw.Stop ()
        value, sw.Elapsed

    let inline countIt amount ([<InlineIfLambda>] f) =
        let sw = System.Diagnostics.Stopwatch.StartNew ()
        let mutable result = Unchecked.defaultof<_>
        for i=1 to amount do
            result <- f ()
        sw.Stop ()
        result, (sw.Elapsed / float amount)
