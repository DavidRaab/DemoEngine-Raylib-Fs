module Test =
    let mutable Accuracy   = 0.0000001
    let mutable Accuracy32 = 0.000001f
    let mutable testsSoFar = 0

    /// Defines how many tests should be runned
    let plan num =
        printfn "1..%d" num

    /// If you don't know how many test will be runned, call doneTesting
    /// at the end of all testing
    let doneTesting () =
        plan testsSoFar

    /// Checks if a boolean is true
    let ok bool name =
        testsSoFar <- testsSoFar + 1
        if bool then printfn "ok %d - %s"     testsSoFar name
                else printfn "not ok %d - %s" testsSoFar name

    /// Compares if two values are the same
    let is got expected name =
        testsSoFar <- testsSoFar + 1
        if got = expected then
            printfn "ok %d - %s" testsSoFar name
        else
            printfn "not ok %d - %s" testsSoFar name
            printfn "#   Failed test '%s'" name
            printfn "#          got: %A" got
            printfn "#     expected: %A" expected

    /// Check if a function successfully throws an exception
    let throws f name =
        let mutable throws = false
        try
            f ()
        with
        | _ -> throws <- true

        testsSoFar <- testsSoFar + 1
        if throws
        then printfn "ok %d - %s" testsSoFar name
        else printfn "not ok %d - %s" testsSoFar name

    /// Explicitly pass a test, used in conjunction with `fail`
    let pass name =
        testsSoFar <- testsSoFar + 1
        printfn "ok %d - %s" testsSoFar name

    /// Explicitly fail a test, used in conjunction with `pass`
    let fail name =
        testsSoFar <- testsSoFar + 1
        printfn "not ok %d - %s" testsSoFar name

    /// Check if two floats are the same with the accuracy of `Test.Accuracy`
    let float got expected name =
        testsSoFar <- testsSoFar + 1
        if   abs (got - expected) < Accuracy
        then printfn "ok %d - %s" testsSoFar name
        else
            printfn "not ok %d - %s" testsSoFar name
            printfn "#   Failed test '%s'" name
            printfn "#          got: %f" got
            printfn "#     expected: %f" expected

    let float32 (got:float32) (expected:float32) name =
        testsSoFar <- testsSoFar + 1
        if   abs (got - expected) < Accuracy32
        then printfn "ok %d - %s" testsSoFar name
        else
            printfn "not ok %d - %s" testsSoFar name
            printfn "#   Failed test '%s'" name
            printfn "#          got: %f" got
            printfn "#     expected: %f" expected

    /// Check if two float lists are identical with the accuracy of `Test.Acurracy`
    let floatList (got:list<float>) expected name =
        let fail name =
            testsSoFar <- testsSoFar + 1
            printfn "not ok %d - %s" testsSoFar name
            printfn "#   Failed test '%s'" name
            printfn "#          got: %A" got
            printfn "#     expected: %A" expected

        let rec loop got expected =
            match got, expected with
            | [],     []          -> pass name
            | x::got, []          -> fail name
            | [],     y::expected -> fail name
            | x::got, y::expected ->
                if   abs (x - y) < Accuracy
                then loop got expected
                else fail name

        loop got expected

    /// Check if two float32 lists are identical with the accuracy of `Test.Acurracy32`
    let float32List (got:list<float32>) expected name =
        let fail name =
            testsSoFar <- testsSoFar + 1
            printfn "not ok %d - %s" testsSoFar name
            printfn "#   Failed test '%s'" name
            printfn "#          got: %A" got
            printfn "#     expected: %A" expected

        let rec loop got expected =
            match got, expected with
            | [],     []          -> pass name
            | x::got, []          -> fail name
            | [],     y::expected -> fail name
            | x::got, y::expected ->
                if   abs (x - y) < Accuracy32
                then loop got expected
                else fail name

        loop got expected

