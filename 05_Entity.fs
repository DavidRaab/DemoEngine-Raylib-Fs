namespace MyGame.Entity
open Dic2
open MyGame
open MyGame.DataTypes
open MyGame.Components
open MyGame.State

module Entity =
    let mutable private counter = 0
    let private entities = ResizeArray<Entity>()

    let create () =
        counter <- counter + 1
        let e = Entity counter
        entities.Add e
        e

    let init f =
        let entity = create ()
        f entity
        entity

    let initMany count f = [
        for idx in 0 .. count-1 do
            let e = create ()
            f idx e
            yield e
    ]

    let all () =
        entities :> seq<Entity>

[<AutoOpen>]
module EntityExtension =
    type Entity with
        member entity.addTransform t     = Dictionary.add    entity t    State.Transform
        member entity.deleteTransform () = Dictionary.remove entity      State.Transform
        member entity.addMovement mov    = Dictionary.add    entity mov  State.Movement
        member entity.deleteMovement ()  = Dictionary.remove entity      State.Movement
        member entity.addAnimation anim  = Dictionary.add    entity anim State.Animation
        member entity.deleteAnimation () = Dictionary.remove entity      State.Animation
        member entity.addView layer view = Dic2.add    layer entity view State.View
        member entity.deleteView ()      = Dic2.remove       entity      State.View
        member entity.setAnimation name =
            match Dictionary.get entity State.Animation with
            | ValueSome anim -> Comp.switchAnimation name anim
            | ValueNone      -> ()

        member entity.getSheetExn name : Sheet =
            match Dictionary.get entity State.Animation with
            | ValueSome anim -> anim.Sheets.Sheets.[name]
            | ValueNone      -> failwithf "%A has no SheetAnimations" entity

