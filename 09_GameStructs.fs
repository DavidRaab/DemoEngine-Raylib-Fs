module MyGame.GameStructs
open Raylib_cs
open System.Numerics
open MyGame.DataTypes
open MyGame.Utility

// Shorter Alias for KeyboardKey
type Key = Raylib_cs.KeyboardKey

// Model
type MouseRectangle =
    | NoRectangle
    | StartRectangle of Vector2
    | DrawRectangle  of Vector2 * Vector2
    | EndRectangle   of Vector2 * Vector2

type Model = {
    Knight:         Entity
    MouseRectangle: MouseRectangle
    Rng:            System.Random
    Boxes:          ResizeArray<Entity>
}

type KnightState =
    | IsAttack of elapsed:float32 * duration:float32
    | IsLeft   of Vector2
    | IsRight  of Vector2
    | IsCrouch
    | IsIdle

let statePriority state =
    match state with
    | IsAttack _ -> 4
    | IsLeft   _ -> 3
    | IsRight  _ -> 3
    | IsCrouch   -> 2
    | IsIdle     -> 1

type Action =
    | Attack
    | MoveLeft  of Vector2
    | MoveRight of Vector2
    | Crouch
    | Movement  of Vector2
    | Camera    of Vector2
    | CameraHome
    | ZoomReset
    | ScrollZoom  of float32
    | ZoomIn
    | ZoomOut
    | DragStart   of Vector2
    | DragBetween of Vector2
    | DragEnd     of Vector2

// Input mapping to User Actions
let inputMapping = {
    Keyboard = [
        Key.Space, IsPressed, Attack
        Key.Space, IsPressed, Attack
        Key.Left,  IsKeyDown, MoveLeft  Vector2.Left
        Key.Right, IsKeyDown, MoveRight Vector2.Right
        Key.Down,  IsKeyDown, Crouch
        Key.W,     IsKeyDown, Camera Vector2.Up
        Key.A,     IsKeyDown, Camera Vector2.Left
        Key.S,     IsKeyDown, Camera Vector2.Down
        Key.D,     IsKeyDown, Camera Vector2.Right
        Key.Z,     IsKeyDown, ZoomReset
        Key.Home,  IsKeyDown, CameraHome
        Key.R,     IsKeyDown, ZoomIn
        Key.F,     IsKeyDown, ZoomOut
    ]
    GamePad = {
        Buttons = [
            GamePad.ButtonLeft, IsPressed, Attack
            GamePad.DPadLeft,   IsKeyDown, MoveLeft  Vector2.Left
            GamePad.DPadRight,  IsKeyDown, MoveRight Vector2.Right
            GamePad.DPadDown,   IsKeyDown, Crouch
        ]
        ThumbStick = {
            Left  = Some Movement
            Right = Some Camera
        }
        Trigger = {
            Left  = Some (fun m -> MoveLeft  (Vector2.Left  * m))
            Right = Some (fun m -> MoveRight (Vector2.Right * m))
        }
    }
    Mouse = {
        Buttons = [
            MouseButton.Left, IsPressed,  World (DragStart)
            MouseButton.Left, IsKeyDown,  Screen(DragBetween)
            MouseButton.Left, IsReleased, World (DragEnd)
        ]
        ScrollWheel = Some (cmpF (is ScrollZoom 1f) (is ScrollZoom -1) (is ScrollZoom 0))
        Position    = None
    }
}
