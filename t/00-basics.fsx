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
let isVector (given:Vector2) (expected:Vector2) name =
    let (x1,y1) =    given.X,    given.Y
    let (x2,y2) = expected.X, expected.Y
    match nearly x1 x2, nearly y1 y2 with
    | true, true -> Test.pass name
    | _          ->
        Test.fail name
        printfn "# Expected (%f,%f) Given (%f,%f) " x2 y2 x1 y1

// Test Vector2.fromAngleDeg
let tests = [
    0f<deg>,    Vector2.Up
    15f<deg>,   Vector2.create 0.258819f -0.965926f
    30f<deg>,   Vector2.create 0.500000f -0.866025f
    45f<deg>,   Vector2.Normalize(Vector2.create 1f -1f) // Top Right
    60f<deg>,   Vector2.create 0.866025f -0.500000f
    75f<deg>,   Vector2.create 0.965926f -0.258819f
    90f<deg>,   Vector2.Right
    135f<deg>,  Vector2.Normalize(Vector2.create 1f 1f) // Bottom Right
    180f<deg>,  Vector2.Down
    225f<deg>,  Vector2.Normalize(Vector2.create -1f 1f) // Bottom Left
    270f<deg>,  Vector2.Left
    315f<deg>,  Vector2.Normalize(Vector2.create -1f -1f) // Top Left
    360f<deg>,  Vector2.Up
    720f<deg>,  Vector2.Up
    -90f<deg>,  Vector2.Left
    -180f<deg>, Vector2.Down
]

// Coordination System in Monogame. Positive X goes right, Positive Y goes down
// But in the game engine i define "Up" to be the logical Up on the screen
// so Up becomes Vector2(1,-1)

// Test AngleDeg for correct values
for (deg,expected) in tests do
    isVector (Vector2.fromAngleDeg deg) expected "Vector2.fromAngleDeg"

// check if AngleDeg and AngleRad return the same vectors for same input
for (deg,_) in tests do
    isVector
        (Vector2.fromAngleDeg deg)
        (Vector2.fromAngleRad (Radian.fromDeg deg))
        "Vector2.fromAngleDeg same as Vector2.fromAngleRad"

// Converting a vector to an angle
let angles = [
    Vector2.Up,    0f
    Vector2.Right, (System.MathF.PI / 2f)
    Vector2.Down,  (System.MathF.PI)
    Vector2.Left,  (-System.MathF.PI / 2f)
    Vector2.create  1f -1f, (Radian.fromDeg   45f<deg> |> float32) // Top Right
    Vector2.create  1f  1f, (Radian.fromDeg  135f<deg> |> float32) // Bottom Right
    Vector2.create -1f  1f, (Radian.fromDeg -135f<deg> |> float32) // Bottom Left
    Vector2.create -1f -1f, (Radian.fromDeg  -45f<deg> |> float32) // Top Left
]
angles |> List.iteri (fun idx (vec,expected) ->
    Test.float32
        (Vector2.angle vec |> float32)
        expected
        (sprintf "Vector2.angle %d" idx)
)