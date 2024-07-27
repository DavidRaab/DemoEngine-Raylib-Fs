namespace MyGame.State
open Raylib_cs
open Dic2
open MyGame
open MyGame.DataTypes

type Dictionary<'a,'b> = System.Collections.Generic.Dictionary<'a,'b>

module State =
    let mutable camera   = Unchecked.defaultof<Camera2D>
    let mutable uiCamera = Unchecked.defaultof<Camera2D>
    let Transform        = Dictionary<Entity,Transform>()
    let Movement         = Dictionary<Entity,Movement>()
    let Animation        = Dictionary<Entity,Animation>()
    // (bool * Layer) represents IsVisible * Layer
    let View             = Dic2.create<struct (bool * Layer), Entity, View>()
