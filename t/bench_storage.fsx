#!/usr/bin/env -S dotnet fsi

#load "Benchmark.fs"
#load "../Lib_Storage.fs"
open System.Numerics
open Benchmark
open Storage

let rng = System.Random ()

// Here i want to benchmark the iterative performance of an array of
// classes (pointer-type) vs structs (value types)

type TransformP = {
    Position: Vector2
    Scale:    Vector2
    Rotation: float32
}

[<Struct>]
type TransformS = {
    Position: Vector2
    Scale:    Vector2
    Rotation: float32
}

// constructors
let createTransformP () : TransformP = {
    Position = Vector2(1f , rng.NextSingle())
    Rotation = rng.NextSingle ()
    Scale    = Vector2.One
}

let createTransformS () : TransformS = {
    Position = Vector2(1f , rng.NextSingle())
    Rotation = rng.NextSingle ()
    Scale    = Vector2.One
}


// Initialize two arrays
let stoP = Storage.create ()
let stoS = Storage.create ()

let amount = 50_000
for i=1 to amount do
    Storage.insert i (createTransformP ()) stoP
for i=1 to amount do
    Storage.insert i (createTransformS ()) stoS

// just iterate through both and count items
let (countP, timeP) = countIt 10_000 (fun () ->
    let mutable count = 0f
    stoP |> Storage.iter (fun k t ->
        count <- count + t.Position.X
    )
    count
)

let (countS, timeS) = countIt 10_000 (fun () ->
    let mutable count = 0f
    stoS |> Storage.iter (fun k t ->
        count <- count + t.Position.X
    )
    count
)

// print results
printfn "Pointer Count: %f Time: %O" countP timeP
printfn "Struct  Count: %f Time: %O" countS timeS

// Try to simulate memory fragmentation
// randomly delete entries from countP and create new ones to add. Currently
// all TransformP should be close in memory as they are created in a loop.
// but during typical runtime transforms will usually created and deleted.
//
// on the other hand, a struct is always packed together. No memory fragementation
// will happen. but for .net it means a struct always must be fully copied on
// index read or index set. So it has a general less overhead compared to
// arrayP, but performance stays the same even after some runing time of the
// application.
