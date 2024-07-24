namespace MyGame.Entity
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
        member entity.addTransform t     = State.Transform.add t entity
        member entity.deleteTransform () = State.Transform.delete entity
        member entity.addView view       = State.View.add entity true view
        member entity.deleteView ()      = State.View.remove entity
        member entity.addMovement mov    = State.Movement.add mov entity
        member entity.deleteMovement ()  = State.Movement.delete entity
        member entity.addAnimation anim  = State.Animation.add anim entity
        member entity.deleteAnimation () = State.Animation.delete entity
        member entity.setAnimation name =
            entity |> State.Animation.fetch (fun anim ->
                Animation.switchAnimation name anim
            )
        member entity.getSheetExn name : Sheet =
            match State.Animation.get entity with
            | ValueSome anim -> anim.Sheets.Sheets.[name]
            | ValueNone      -> failwithf "%A has no SheetAnimations" entity

