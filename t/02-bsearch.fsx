#!/usr/bin/env -S dotnet fsi

#r "nuget:Raylib-cs"
#load "Test.fsx"
#load "../01_Extensions.fs"
open Test
open MyGame

let data = [|1; 5; 20; 60; 100; 101; 200; 250; 299|]

for idx,value in Array.indexed data do
    Test.is (Array.bsearch value data) (ValueSome idx) "Check bsearch"

Test.doneTesting ()