namespace MyGame
open Storage
open Sto2

// I thought that was not needed anymore, but it seems because Entity is
// a struct this must be added.
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Entity =
    let mutable private counter = 0

    let create () =
        counter <- counter + 1
        let e = Entity counter
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

    let inline addTransform t entity =
        Storage.add entity t State.Transform

    let inline getTransform entity =
        Storage.get entity State.Transform

    let deleteTransform entity =
        Storage.remove entity State.Transform

    let addAnimation  anim entity = Storage.add          entity anim State.Animation
    let deleteAnimation    entity = Storage.remove       entity      State.Animation
    let addView layer view entity = Sto2.add       layer entity view State.View
    let deleteView         entity = Sto2.remove          entity      State.View

    let setAnimation name entity =
        match Storage.get entity State.Animation with
        | ValueSome anim -> Comp.switchAnimation name anim
        | ValueNone      -> ()

    let getSheetExn name entity : Sheet =
        match Storage.get entity State.Animation with
        | ValueSome anim -> anim.Sheets.Sheets.[name]
        | ValueNone      -> failwithf "%A has no SheetAnimations" entity

    /// Destroys an entity by removing all Components
    let destroy entity =
        Storage.remove entity State.Transform
        Sto2.remove    entity State.View
        Storage.remove entity State.Animation
        Storage.remove entity State.AutoMovement
        Storage.remove entity State.AutoTargetPosition
