namespace MyGame.State
open Raylib_cs
open Dic2
open MyGame
open MyGame.DataTypes

type Dictionary<'a,'b> = System.Collections.Generic.Dictionary<'a,'b>

module State =
    /// Stores information on how many DrawTexture() calls
    /// happened in draw. Resets every frame.
    let mutable drawed   = 0

    // Camera objects, initialized on game startup
    let mutable camera   = Unchecked.defaultof<Camera2D>
    let mutable uiCamera = Unchecked.defaultof<Camera2D>

    // State variables
    let Transform = Dictionary<Entity,Transform>(50000)
    let Movement  = Dictionary<Entity,Movement>(50000)
    let Animation = Dictionary<Entity,Animation>(50000)
    let View      = Dic2.create<Layer, Entity, View>()
