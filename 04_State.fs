namespace MyGame.State
open Raylib_cs
open Storage
open Sto2
open MyGame
open MyGame.DataTypes

type Dictionary<'a,'b> = System.Collections.Generic.Dictionary<'a,'b>


module State =
    /// Initialized Random Number Generator to be used
    let rng = System.Random ()

    /// Stores information on how many DrawTexture() calls
    /// happened in draw. Resets every frame.
    let mutable drawed   = 0

    // Camera objects, initialized on game startup
    let mutable camera   = Unchecked.defaultof<Camera2D>
    let mutable uiCamera = Unchecked.defaultof<Camera2D>

    // State variables
    // let Transform = Dic2.create<TransformState, Entity, Transform> ()
    let TransformLocal  = Storage.create<Entity,Transform> ()
    let TransformParent = Storage.create<Entity,Transform> ()
    let Movement        = Storage.create<Entity,Movement>  ()
    let Animation       = Storage.create<Entity,Animation> ()
    let View            = Sto2.create<Layer, Entity, View> ()

