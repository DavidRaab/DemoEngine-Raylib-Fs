#!/usr/bin/env -S dotnet fsi
#r "nuget:MonoGame.Framework.DesktopGL"
#load "../01_Extensions.fs"
#load "../02_DataTypes.fs"
#load "../03_Components.fs"
#load "Test.fsx"

open Microsoft.Xna.Framework
open Microsoft.Xna.Framework.Graphics
open Test
open MyGame
open MyGame.Extensions
open MyGame.DataTypes
open MyGame.Components

// checks if two floats are nearly the same.
let nearly (x:float32) (y:float32) =
    (abs (x - y)) < 0.0001f

// checks if two vectors are nearly the same
let isVector (got:Vector2) (expected:Vector2) name =
    let (x1,y1) =    got.X,    got.Y
    let (x2,y2) = expected.X, expected.Y
    match nearly x1 x2, nearly y1 y2 with
    | true, true -> Test.pass name
    | _          ->
        Test.fail name
        printfn "# Expected (%f,%f) Given (%f,%f) " x2 y2 x1 y1

let rad x =
    Radian.fromDeg x |> float32

let pOff = Vector3(10f,10f,0f)

Test.float32 (rad 45f<deg>) 0.785398f "45 degree to radian"
Test.float32
    (Radian.fromDeg 45f<deg> |> float32)
    ((Radian.fromDeg (Radian.toDeg (Radian.fromDeg 45f<deg>))) |> float32)
    "fromDeg toDeg is isomorph"

// (10,0) on parent (10,10) rot 0°
let pos1 = Vector2.Transform(
    (Vector2.create 10f 0f),
    Matrix.CreateRotationZ(rad 0f<deg>)
    * Matrix.CreateTranslation(pOff)
)
isVector pos1 (Vector2.create 20f 10f) "(10,0) on parent (10,10) 0°"

// (10,0) on parent (10,10) rot 45°
let pos2 = Vector2.Transform(
    (Vector2.create 10f 0f),
    Matrix.CreateRotationZ(rad 45f<deg>) // Parent Rotation
    * Matrix.CreateTranslation(pOff)     // Parent offset
)
isVector pos2 (Vector2.create 17.071068f 17.071068f) "(10,0) on parent (10,10) 45°"

// (10,0) on parent (10,10) rot 90°
let pos3 = Vector2.Transform(
    (Vector2.create 10f 0f),
    Matrix.CreateRotationZ(rad 90f<deg>) // Parent Rotation
    * Matrix.CreateTranslation(pOff)     // Parent offset
)
isVector pos3 (Vector2.create 10f 20f) "(10,0) on parent (10,10) 90°"

// assume pos1 has parent (10,10) with rotation 45°
// let pos4 = Vector2.Transform(
//     Vector2.Zero,
//     Matrix.CreateScale(1f, 1f, 0f)
//     * Matrix.CreateTranslation(Vector3(10f,0f,0f))
//     * Matrix.CreateRotationZ(float32 (Radian.fromDeg 45f<deg>))
//     * Matrix.CreateTranslation(-Vector3(10f,0f,0f))             // subtract local pos
//     * Matrix.CreateTranslation(Vector3(10f,10f,0f))             // Parent translation
//     * Matrix.CreateRotationZ(float32 (Radian.fromDeg 45f<deg>)) // parent rotation
//     * Matrix.CreateTranslation(Vector3(10f,0f,0f))              // add local pos
// )
