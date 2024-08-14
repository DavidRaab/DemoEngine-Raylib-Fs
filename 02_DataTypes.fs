namespace MyGame.DataTypes
open Raylib_cs
open System.Numerics
open MyGame.Extensions

type TimeSpan = System.TimeSpan

[<Struct>]
type Entity =
    Entity of int

type LocalTransform = {
    mutable Position: Vector2
    mutable Scale:    Vector2
    mutable Rotation: float32<deg>
}

type ParentTransform = {
    Parent: Entity
    mutable Position:       Vector2
    mutable GlobalPosition: Vector2
    mutable Scale:          Vector2
    mutable GlobalScale:    Vector2
    mutable Rotation:       float32<deg>
    mutable GlobalRotation: float32<deg>
}

type Transform =
    | Local  of LocalTransform
    | Parent of ParentTransform
    with
        member inline self.Position
            with get ()      =
                match self with
                | Local  t -> t.Position
                | Parent t -> t.Position
            and  set (value) =
                match self with
                | Local  t -> t.Position <- value
                | Parent t -> t.Position <- value

        member inline self.Rotation
            with get () =
                match self with
                | Local  t -> t.Rotation
                | Parent t -> t.Rotation
            and set(value) =
                match self with
                | Local  t -> t.Rotation <- value
                | Parent t -> t.Rotation <- value

        member inline self.Scale
            with get () =
                match self with
                | Local  t -> t.Scale
                | Parent t -> t.Scale
            and set (value) =
                match self with
                | Local  t -> t.Scale <- value
                | Parent t -> t.Scale <- value

        member inline self.GlobalPosition
            with get () = match self with | Local t -> t.Position | Parent t -> t.GlobalPosition
        member inline self.GlobalRotation
            with get () = match self with | Local t -> t.Rotation | Parent t -> t.GlobalRotation
        member inline self.GlobalScale
            with get () = match self with | Local t -> t.Scale    | Parent t -> t.GlobalScale

[<Struct>]
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
type Sprite = {
    Texture: Texture2D
    SrcRect: Rectangle
}

/// View component. An Entity needs this component to be shown on screen
type View = {
    mutable Sprite:   Sprite
    mutable Rotation: float32<deg>
    mutable Tint:     Color
    mutable Scale:    Vector2
    Origin: Vector2
}

/// A collection of multiple sprites used in an Animation
type Sheet = {
    Sprites:       Sprite array
    FrameDuration: float32
    IsLoop:        bool
}

type Sheets = {
    Sheets:  Map<string,Sheet>
    Default: string
}

type Animation = {
    Sheets:                Sheets
    mutable CurrentSheet:  Sheet
    mutable ElapsedTime:   float32
    mutable CurrentSprite: int
    mutable MaxSprites:    int
}

type Movement = {
    Direction : MovementDirection voption
    Rotation  : float32<deg>      voption
}
and MovementDirection =
    | Relative of Vector2
    | Absolute of position:Vector2 * speed:float32
