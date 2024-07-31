#!/usr/bin/env -S dotnet fsi

#load "Test.fsx"
#load "../Lib_Storage.fs"

open Test
open Storage

let amount = 50_000
let rng    = System.Random()

let countIs amount storage name =
    Test.is (storage.Data.Count)       amount (sprintf "%s data Count is %d" name amount)
    Test.is (storage.IndexToKey.Count) amount (sprintf "%s IndexToKey Count is %d" name amount)
    Test.is (storage.KeyToIndex.Count) amount (sprintf "%s KeyToIndex Count is %d" name amount)

// generate storage
let storage = Storage.create ()
for i=1 to amount do
    Storage.insert i (rng.NextDouble ()) storage

countIs amount storage "init"

// get value for index 10
let valueOf10   = (Storage.get 10    storage).Value
let valueOfLast = (Storage.get 50000 storage).Value

Test.is (storage.Data.[9])        valueOf10 "check element 10"
Test.is (storage.Data.[49_999]) valueOfLast "check element 50000"
Test.is (Storage.getIndex    10 storage) (ValueSome 9)     "index of 10"
Test.is (Storage.getIndex 50000 storage) (ValueSome 49999) "index of 50000"

// Removes one element
Storage.remove 10 storage

countIs (amount-1) storage "one removed"
Test.is (storage.Data.[9]) valueOfLast "last moved in ereased slot"
Test.is (Storage.getIndex 50000 storage) (ValueSome 9) "Moved to index 9"
Test.is (Storage.getIndex    10 storage)   (ValueNone) "10 removed"

// Add element 10 back
Storage.insert 10 valueOf10 storage

countIs amount storage "50000"
Test.is (storage.Data.[49999]) valueOf10 "added to end"
Test.is (Storage.getIndex 50000 storage) (ValueSome 9) "stays at index 9"
Test.is (Storage.getIndex    10 storage) (ValueSome 49999) "index of 10"
Test.is (Storage.get 10 storage)    (ValueSome valueOf10) "valueof10"
Test.is (Storage.get 50000 storage) (ValueSome valueOfLast) "valueofLast"

// overwrite 10 with new value
Storage.insert 10 (-valueOf10) storage
countIs amount storage "not added - overwritten"
Test.is (storage.Data.[49999]) (-valueOf10) "added to end"
Test.is (Storage.getIndex 10 storage) (ValueSome 49999) "index of 10"

// delete last element in Data that is now 10
Storage.remove 10 storage
countIs (amount-1) storage "delete last"
Test.is (Storage.get 10 storage) (ValueNone) "10 not in storage"

// fetch keys,values from 101 to 200 and delete them
let kv = [
    for key in 101 .. 200 do
        let value = (Storage.get key storage).Value
        Storage.remove key storage
        yield key,value
]
countIs (amount-101) storage "deleted 100"

// now re-add the elements
for (key,value) in kv do
    Storage.insert key value storage

countIs (amount-1) storage "re-added 100"

// check all values
for (key,value) in kv do
    Test.is (Storage.get key storage) (ValueSome value) (sprintf "Value of %d" key)

// re-add 10
Storage.insert 10 valueOf10 storage

countIs amount storage "added 10 back"
Test.is (Storage.getIndex 10 storage) (ValueSome 49999) "last element"
Test.is (Storage.get 10 storage)      (ValueSome valueOf10) "value of 10"
Test.is (Storage.get 50000 storage)   (ValueSome valueOfLast) "last did not change"
Test.is (Storage.getIndex 50000 storage) (ValueSome 9) "50000 still at index 9"


for i=1 to 100 do
    let rKey = rng.Next(50000) + 1
    Test.is
        (Storage.getKey ((Storage.getIndex rKey storage).Value) storage)
        (ValueSome rKey)
        "roundTrip"

Test.doneTesting ()