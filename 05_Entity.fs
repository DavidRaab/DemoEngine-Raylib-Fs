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

    let addTransform t entity =
        match t with
        | Local _ ->
            Dictionary.remove entity   State.TransformParent
            Dictionary.add    entity t State.TransformLocal
        | Parent _ ->
            Dictionary.remove entity   State.TransformLocal
            Dictionary.add    entity t State.TransformParent

    let getTransform entity =
        match Dictionary.get entity State.TransformLocal with
        | ValueSome t -> ValueSome t
        | ValueNone   ->
            match Dictionary.get entity State.TransformParent with
            | ValueSome t -> ValueSome t
            | ValueNone   -> ValueNone

    let deleteTransform entity =
        Dictionary.remove entity State.TransformLocal
        Dictionary.remove entity State.TransformParent

    let addMovement   mov  entity = Dictionary.add    entity mov  State.Movement
    let deleteMovement     entity = Dictionary.remove entity      State.Movement
    let addAnimation  anim entity = Dictionary.add    entity anim State.Animation
    let deleteAnimation    entity = Dictionary.remove entity      State.Animation
    let addView layer view entity = Dic2.add    layer entity view State.View
    let deleteView         entity = Dic2.remove       entity      State.View

    let setAnimation name entity =
        match Dictionary.get entity State.Animation with
        | ValueSome anim -> Comp.switchAnimation name anim
        | ValueNone      -> ()

    let getSheetExn name entity : Sheet =
        match Dictionary.get entity State.Animation with
        | ValueSome anim -> anim.Sheets.Sheets.[name]
        | ValueNone      -> failwithf "%A has no SheetAnimations" entity
