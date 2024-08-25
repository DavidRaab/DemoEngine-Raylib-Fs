namespace MyGame
open Raylib_cs
open System.Numerics

[<Struct;NoComparison>]
type Entity =
    Entity of int

[<NoEquality;NoComparison>]
type Transform = {
    mutable Parent:         voption<Entity>
    mutable Position:       Vector2
    mutable GlobalPosition: Vector2
    mutable Scale:          Vector2
    mutable GlobalScale:    Vector2
    mutable Rotation:       float32<deg>
    mutable GlobalRotation: float32<deg>
}

[<Struct;NoComparison>]
type Origin =
    | TopLeft
    | Top
    | TopRight
    | Left
    | Center
    | Right
    | BottomLeft
    | Bottom
    | BottomRight
    | Position of float32 * float32

/// The Layer GameObjects are Drawn to. Divided into Background and Foreground.
/// The number indicates a Depth. Example: So BG3 is further away than BG1.
/// This means elements in BG1 are always drawn on top of BG3.
type Layer =
    | BG3 = 0uy
    | BG2 = 1uy
    | BG1 = 2uy
    | FG3 = 3uy
    | FG2 = 4uy
    | FG1 = 5uy

/// A sprite is a selection from a Texture2D
[<NoEquality;NoComparison>]
type Sprite = {
    Texture: Texture2D
    SrcRect: Rectangle
}

/// View component. An Entity needs this component to be shown on screen
[<NoEquality;NoComparison>]
type View = {
    mutable Sprite:   Sprite
    mutable Rotation: float32<deg>
    mutable Tint:     Color
    mutable Scale:    Vector2
    Origin: Vector2
}

/// A collection of multiple sprites used in an Animation
[<NoEquality;NoComparison>]
type Sheet = {
    Sprites:       Sprite array
    FrameDuration: float32
    IsLoop:        bool
}

[<NoEquality;NoComparison>]
type Sheets = {
    Sheets:  Map<string,Sheet>
    Default: string
}

[<NoEquality;NoComparison>]
type Animation = {
    Sheets:                Sheets
    mutable CurrentSheet:  Sheet
    mutable ElapsedTime:   float32
    mutable CurrentSprite: int
    mutable MaxSprites:    int
}

[<NoEquality;NoComparison>]
type AutoMovement = {
    mutable Direction : Vector2
    mutable RotateBy  : float32<deg>
}

[<NoEquality;NoComparison>]
type AutoTargetPosition = {
    mutable Position : Vector2
    mutable Speed    : float32
}

type PathWalkingState =
    | NotStarted = 0
    | Walking    = 1
    | Finished   = 2

[<NoEquality;NoComparison>]
type PathWalking = {
    Path:  Vector2 array
    Speed: float32
    mutable State:     PathWalkingState
    mutable Current:   int
    mutable Direction: Vector2 // Already has the Speed applied
}