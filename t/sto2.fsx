#!/usr/bin/env -S dotnet fsi

#load "Test.fsx"
#load "../Lib_Storage.fs"
#load "../Lib_Sto2.fs"
open Test
open Sto2

type Layer =
    | BG1
    | BG2
    | BG3
    | BG4

[<Struct>]
type Entity = Entity of int

let data = Sto2.create<Layer,Entity,int>()

let show dic =
    dic |> Sto2.iter BG1 (fun e v -> printfn "BG1 %A %d" e v)
    dic |> Sto2.iter BG2 (fun e v -> printfn "BG2 %A %d" e v)
    dic |> Sto2.iter BG3 (fun e v -> printfn "BG3 %A %d" e v)

let checkContent data name bav =
    for (keyB,keyA,value) in bav do
        let testname = sprintf "%s - get %A" name keyB
        Test.is (Sto2.get keyB data) (ValueSome (keyA,value)) testname

let checkCount amount data =
    Test.is (Sto2.count data) (data.BToA.Count) "Compare internal Count"
    Test.is (Sto2.count data) amount $"Count is {amount}"

// Add and check count size
data |> Sto2.add BG1 (Entity 1) 10
checkCount 1 data

data |> Sto2.add BG2 (Entity 2) 20
checkCount 2 data

data |> Sto2.add BG3 (Entity 3) 30
checkCount 3 data

data |> Sto2.add BG1 (Entity 4) 40
checkCount 4 data

checkContent data "c1" [
    (Entity 1), BG1, 10
    (Entity 2), BG2, 20
    (Entity 3), BG3, 30
    (Entity 4), BG1, 40
]

Sto2.add BG2 (Entity 4) 40 data
checkContent data "c2" [
    (Entity 1), BG1, 10
    (Entity 2), BG2, 20
    (Entity 3), BG3, 30
    (Entity 4), BG2, 40
]

Sto2.add BG2 (Entity 4) 50 data
checkContent data "c3" [
    (Entity 1), BG1, 10
    (Entity 2), BG2, 20
    (Entity 3), BG3, 30
    (Entity 4), BG2, 50
]

List.iter (fun (l,e,v) -> Sto2.add l e v data) [
    BG2, Entity 1, 100
    BG3, Entity 2, 110
    BG1, Entity 4, 120
    BG1, Entity 3, 140
    BG2, Entity 5, 150
]

checkCount 5 data
checkContent data "c4" [
    Entity 1, BG2, 100
    Entity 2, BG3, 110
    Entity 4, BG1, 120
    Entity 3, BG1, 140
    Entity 5, BG2, 150
]

Sto2.remove (Entity 3) data
checkCount 4 data
checkContent data "c5" [
    Entity 1, BG2, 100
    Entity 2, BG3, 110
    Entity 4, BG1, 120
    Entity 5, BG2, 150
]

Sto2.clear data
checkCount 0 data

List.iter (fun (l,e,v) -> Sto2.add l e v data) [
    BG1, Entity 4, 120
    BG1, Entity 3, 140
    BG4, Entity 2, 100
]
checkCount 3 data
checkContent data "after clear" [
    Entity 2, BG4, 100
    Entity 3, BG1, 140
    Entity 4, BG1, 120
]

Test.is (Sto2.get (Entity 1) data) ValueNone "not exists"

let mutable count = 0
data |> Sto2.iter BG2 (fun e v -> count <- count + 1)
Test.is count 0 "no error and stays 0"

Test.doneTesting ()
