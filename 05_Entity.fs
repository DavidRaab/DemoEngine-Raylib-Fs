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

    let addTransform t entity =
        match t with
        | Local _ ->
            Storage.remove entity   State.TransformParent
            Storage.insert entity t State.TransformLocal
        | Parent _ ->
            Storage.remove entity   State.TransformLocal
            Storage.insert entity t State.TransformParent

    // I don't like it that it needs to try to fetch from one storage and then
    // from another storage. I could use Sto2, but it shouldn't have any advantage.
    // Even in Sto2 i need to fetch the mapping and then fetch again. With exactly
    // asking two Storage this should be fine. But maybe someday i have a better
    // optimization for it?
    let getTransform entity =
        match Storage.get entity State.TransformLocal with
        | ValueSome t -> ValueSome t
        | ValueNone   ->
            match Storage.get entity State.TransformParent with
            | ValueSome t -> ValueSome t
            | ValueNone   -> ValueNone

    let deleteTransform entity =
        Storage.remove entity State.TransformLocal
        Storage.remove entity State.TransformParent

    let addMovement   mov  entity = Storage.insert entity mov  State.Movement
    let deleteMovement     entity = Storage.remove entity      State.Movement
    let addAnimation  anim entity = Storage.insert entity anim State.Animation
    let deleteAnimation    entity = Storage.remove entity      State.Animation
    let addView layer view entity = Sto2.add    layer entity view State.View
    let deleteView         entity = Sto2.remove       entity      State.View

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
        deleteTransform entity
        deleteView entity
        deleteMovement entity
        deleteAnimation entity
