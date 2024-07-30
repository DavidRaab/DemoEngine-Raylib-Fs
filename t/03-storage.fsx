#!/usr/bin/env -S dotnet fsi

#r "nuget:Raylib-cs"
#load "Test.fsx"
#load "../01_Extensions.fs"
#load "../Lib_Storage.fs"
open Test
open MyGame
open Storage

// function for checking the content of Storage
let checkContent name data content =
    let mutable idx  = 0
    let mutable pass = true
    if Seq.length content = Storage.length data then
        for key,value in content do
            if data.Keys.[idx] <> key then
                printfn "# key mismatch at idx %d: Expecting: %A Got: %A" idx key data.Keys.[idx]
                pass <- false
            if data.Values.[idx] <> value then
                printfn "# value mismatch at idx %d: Expecting: %A Got: %A" idx value data.Values.[idx]
                pass <- false
            idx <- idx + 1
    else
        printfn "# Storage has not required length. Skipping key/value tests."
        pass <- false
    if pass
    then Test.pass name
    else Test.fail name

// Build and Test Storage container
let sto = Storage.create<string,int>()
checkContent "empty" sto []

sto |> Storage.insert "foo" 1
sto |> Storage.insert "bar" 2
sto |> Storage.insert "abz" 3
checkContent "init" sto ["abz", 3; "bar", 2; "foo", 1]

sto |> Storage.insert "foo" 5
checkContent "overwritten foo" sto ["abz", 3; "bar", 2; "foo", 5]

sto |> Storage.insert "aaa" 11
checkContent "added aaa" sto ["aaa",11; "abz", 3; "bar", 2; "foo", 5]

sto |> Storage.insert "za" 100
sto |> Storage.insert "zb" 1
checkContent "adding to end" sto ["aaa",11; "abz", 3; "bar", 2; "foo", 5; "za",100; "zb",1]

sto |> Storage.remove "aaa"
sto |> Storage.remove "foo"
checkContent "removing entries" sto ["abz", 3; "bar", 2; "za",100; "zb",1]

let concatString =
    let mutable str = ""
    sto |> Storage.iter (fun k v -> str <- str + k)
    str
Test.is concatString "abzbarzazb" "keys concatenated"

let addedVals =
    let mutable sum = 0
    sto |> Storage.iter (fun k v -> sum <- sum + v)
    sum
Test.is addedVals 106 "added values"

Test.is (Storage.get "abz" sto) (ValueSome   3) "abz is 3"
Test.is (Storage.get "bar" sto) (ValueSome   2) "bar is 2"
Test.is (Storage.get "za"  sto) (ValueSome 100) "za is 2"
Test.is (Storage.get "zb"  sto) (ValueSome   1) "zb is 1"
Test.is (Storage.get "foo" sto)      ValueNone  "foo does not exists"

Test.doneTesting ()