namespace MyGame.Systems
open Raylib_cs
open System.Numerics
open MyGame
open MyGame.Extensions
open MyGame.DataTypes
open MyGame.Components
open MyGame.State
open MyGame.Entity
open MyGame.Timer

type TimeSpan = System.TimeSpan

// View System draws entity
module View =
    // Calculates position, rotation and scale relative to parent
    // returns an voption because it is called recursively on transform. ValueNone
    // indicates when a parent has no transform defined and recursion ends.
    let rec calculateTransform (me:Transform) =
        match me.Parent with
        | ValueNone        -> ValueSome (me.Position,me.Rotation,me.Scale)
        | ValueSome parent ->
            match State.Transform.get parent with
            | ValueNone        -> ValueNone
            | ValueSome parent ->
                (calculateTransform parent) |> ValueOption.map (fun (pPos,pRot,pScale) ->
                    let scale = Vector2.create (pScale.X * me.Scale.X) (pScale.Y * me.Scale.Y)
                    let pos   = Vector2.Transform(
                        me.Position,
                        Matrix.CreateScale(scale.X, scale.Y, 0f)
                        * Matrix.CreateRotationZ(float32 (Radian.fromDeg pRot)) // rotate by parent position
                        * Matrix.CreateTranslation(Vector3(pPos,0f))            // translate by parent position
                    )
                    pos,pRot+me.Rotation,scale
                )

    let draw () =
        let transformAndView = [|
            for KeyValue(entity,v) in State.View.visible do
                match State.Transform.get entity with
                | ValueSome t -> (t,v)
                | ValueNone   -> ()
        |]

        transformAndView |> Array.sortInPlaceBy (fun (_,v) -> v.Layer)
        for transform,view in transformAndView do
            match calculateTransform transform with
            | ValueNone                           -> ()
            | ValueSome (position,rotation,scale) ->
                Raylib.DrawTexturePro(
                    texture  = view.Sprite.Texture,
                    source   = view.Sprite.SrcRect,
                    dest     = Rectangle(position, view.Sprite.SrcRect.Width * scale.X * view.Scale.X, view.Sprite.SrcRect.Height * scale.Y * view.Scale.Y),
                    origin   = view.Origin,
                    rotation = float32 (rotation + view.Rotation),
                    tint     = view.Tint
                )

// Moves those who should be moved
module Movement =
    let update (deltaTime:TimeSpan) =
        let fdt = float32 deltaTime.TotalSeconds
        for KeyValue(entity,mov) in State.Movement.Data do
            entity |> State.Transform.fetch (fun t ->
                match mov.Direction with
                | ValueNone                        -> ()
                | ValueSome (Relative dir)         -> Transform.addPosition (dir * fdt) t
                | ValueSome (Absolute (pos,speed)) ->
                    let dir = (Vector2.Normalize (pos - t.Position)) * speed
                    Transform.addPosition (dir * fdt) t

                match mov.Rotation with
                | ValueNone     -> ()
                | ValueSome rot -> Transform.addRotation (rot * fdt) t
            )

module Timer =
    let mutable state = ResizeArray<Timed<unit>>()

    let addTimer timer =
        state.Add (Timed.get timer)

    let update (deltaTime:TimeSpan) =
        for idx=0 to state.Count-1 do
            match Timed.run deltaTime (state.[idx]) with
            | Pending    -> ()
            | Finished _ -> state.RemoveAt(idx)

module Animations =
    let update (deltaTime: TimeSpan) =
        for KeyValue(entity,anim) in State.Animation.Data do
            anim.ElapsedTime <- anim.ElapsedTime + deltaTime
            if anim.ElapsedTime > anim.CurrentSheet.FrameDuration then
                anim.ElapsedTime <- anim.ElapsedTime - anim.CurrentSheet.FrameDuration
                Animation.nextSprite anim
                entity |> State.View.iter (fun view ->
                    Animation.updateView view anim
                )

module Drawing =
    let mousePosition mousePos fontSize (whereToDraw:Vector2) =
        let world = Camera.screenToWorld mousePos State.uiCamera
        Raylib.DrawText(
            text     = System.String.Format("Mouse Screen({0},{1}) World({2:0.00},{3:0.00})", mousePos.X, mousePos.Y, world.X, world.Y),
            posX     = int whereToDraw.X,
            posY     = int whereToDraw.Y,
            fontSize = fontSize,
            color    = Color.Black
        )

    let trackPosition (entity:Entity) fontSize (whereToDraw:Vector2) =
        entity |> State.Transform.fetch (fun t ->
            let screen = Camera.worldToScreen t.Position State.uiCamera
            Raylib.DrawText(
                text =
                    System.String.Format("World({0:0.00},{1:0.00}) Screen({2:0},{3:0})",
                        t.Position.X, t.Position.Y,
                        screen.X, screen.Y
                    ),
                posX = int whereToDraw.X,
                posY = int whereToDraw.Y,
                fontSize = fontSize,
                color    = Color.Black
            )
        )

    let line (thickness:float32) color (start:Vector2) (stop:Vector2) =
        Raylib.DrawLineEx(start, stop, thickness, color)

    let rectangle (thickness:int) (color:Color) (topLeft:Vector2) (bottomRight:Vector2) =
        let width  = bottomRight.X - topLeft.X
        let height = bottomRight.Y - topLeft.Y
        Raylib.DrawRectangle(
            int topLeft.X, int topLeft.Y,
            int width, int height,
            Raylib.ColorAlpha(color, 0.1f)
        )
        Raylib.DrawRectangleLinesEx(
            Rectangle(topLeft, width, height), float32 thickness, color
        )
