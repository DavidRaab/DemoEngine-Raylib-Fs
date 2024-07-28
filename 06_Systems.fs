namespace MyGame.Systems
open Raylib_cs
open System.Numerics
open Dic2
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
            match Dictionary.get parent State.Transform with
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

    // Special variables used for object culling. I assume that the camera is
    // always centered. Mean a camera that target world position 0,0 shows this
    // point at the center of the camera. From this point only objects that
    // are half the screen width to any direction away will be rendered. + some
    // offset so sprites don't disappear.
    // Whenever a new frame starts to render, the (min|max)(X|Y) are updated.
    // The offset and halfScreen should be set at program start. halfScreen
    // should be the half of the virtualScreen
    let mutable halfX  = 360f
    let mutable halfY  = 180f
    let mutable offset = 64f
    let mutable minX   = 0f
    let mutable maxX   = 0f
    let mutable minY   = 0f
    let mutable maxY   = 0f

    let inline drawTexture transform view =
        match calculateTransform transform with
            | ValueNone                           -> ()
            | ValueSome (pos,rotation,scale) ->
                if pos.X > minX && pos.X < maxX && pos.Y > minY && pos.Y < maxY then
                    State.drawed <- State.drawed + 1
                    Raylib.DrawTexturePro(
                        texture  = view.Sprite.Texture,
                        source   = view.Sprite.SrcRect,
                        dest     = Rectangle(pos, view.Sprite.SrcRect.Width * scale.X * view.Scale.X, view.Sprite.SrcRect.Height * scale.Y * view.Scale.Y),
                        origin   = view.Origin,
                        rotation = float32 (rotation + view.Rotation),
                        tint     = view.Tint
                    )

    let draw () =
        // Used to track how many objects were really drawn
        State.drawed <- 0

        // Some simple object culling, just camera center + some offset so objects
        // that are on the edge of screen don't dissapear when they hit the edge.
        // But would be a cool Retro effect of early 3D games.
        let cam  = State.camera.Target
        let zoom = State.camera.Zoom
        minX <- cam.X - ((halfX + offset) * (1f / zoom))
        maxX <- cam.X + ((halfX + offset) * (1f / zoom))
        minY <- cam.Y - ((halfY + offset) * (1f / zoom))
        maxY <- cam.Y + ((halfY + offset) * (1f / zoom))

        State.View |> Dic2.iter (true,Layer.BG3) (fun entity v ->
            match Dictionary.get entity State.Transform with
            | ValueSome t -> drawTexture t v
            | ValueNone   -> ()
        )

        State.View |> Dic2.iter (true,Layer.BG2) (fun entity v ->
            match Dictionary.get entity State.Transform with
            | ValueSome t -> drawTexture t v
            | ValueNone   -> ()
        )

        State.View |> Dic2.iter (true,Layer.BG1) (fun entity v ->
            match Dictionary.get entity State.Transform with
            | ValueSome t -> drawTexture t v
            | ValueNone   -> ()
        )

        State.View |> Dic2.iter (true,Layer.FG3) (fun entity v ->
            match Dictionary.get entity State.Transform with
            | ValueSome t -> drawTexture t v
            | ValueNone   -> ()
        )

        State.View |> Dic2.iter (true,Layer.FG2) (fun entity v ->
            match Dictionary.get entity State.Transform with
            | ValueSome t -> drawTexture t v
            | ValueNone   -> ()
        )

        State.View |> Dic2.iter (true,Layer.FG1) (fun entity v ->
            match Dictionary.get entity State.Transform with
            | ValueSome t -> drawTexture t v
            | ValueNone   -> ()
        )


// Moves those who should be moved
module Movement =
    let update (deltaTime:float32) =
        for KeyValue(entity,mov) in State.Movement do
            match Dictionary.get entity State.Transform with
            | ValueSome t ->
                match mov.Direction with
                | ValueNone                        -> ()
                | ValueSome (Relative dir)         -> Transform.addPosition (dir * deltaTime) t
                | ValueSome (Absolute (pos,speed)) ->
                    let dir = (Vector2.Normalize (pos - t.Position)) * speed
                    Transform.addPosition (dir * deltaTime) t

                match mov.Rotation with
                | ValueNone     -> ()
                | ValueSome rot -> Transform.addRotation (rot * deltaTime) t
            | ValueNone ->
                ()

module Timer =
    let mutable state = ResizeArray<Timed<unit>>()

    let addTimer timer =
        state.Add (Timed.get timer)

    let update (deltaTime:float32) =
        let deltaTime = TimeSpan.FromSeconds(float deltaTime)
        for idx=0 to state.Count-1 do
            match Timed.run deltaTime (state.[idx]) with
            | Pending    -> ()
            | Finished _ -> state.RemoveAt(idx)

module Animations =
    let update (deltaTime:float32) =
        let deltaTime = TimeSpan.FromSeconds(float deltaTime)
        for KeyValue(entity,anim) in State.Animation do
            anim.ElapsedTime <- anim.ElapsedTime + deltaTime
            if anim.ElapsedTime > anim.CurrentSheet.FrameDuration then
                anim.ElapsedTime <- anim.ElapsedTime - anim.CurrentSheet.FrameDuration
                Animation.nextSprite anim
                match Dic2.get entity State.View with
                | ValueSome (_,view) -> Animation.updateView view anim
                | ValueNone          -> ()


module Drawing =
    let mousePosition mousePos fontSize (whereToDraw:Vector2) =
        let world = Raylib.GetScreenToWorld2D(mousePos, State.camera)
        Raylib.DrawText(
            text     = System.String.Format("Mouse Screen({0:0.00},{1:0.00}) World({2:0.00},{3:0.00})", mousePos.X, mousePos.Y, world.X, world.Y),
            posX     = int whereToDraw.X,
            posY     = int whereToDraw.Y,
            fontSize = fontSize,
            color    = Color.Yellow
        )

    let trackPosition (entity:Entity) fontSize (whereToDraw:Vector2) =
        match Dictionary.get entity State.Transform with
        | ValueSome t ->
            let screen = Raylib.GetWorldToScreen2D(t.Position, State.camera)
            Raylib.DrawText(
                text =
                    System.String.Format("World({0:0.00},{1:0.00}) Screen({2:0.00},{3:0.00})",
                        t.Position.X, t.Position.Y,
                        screen.X, screen.Y
                    ),
                posX = int whereToDraw.X,
                posY = int whereToDraw.Y,
                fontSize = fontSize,
                color    = Color.Yellow
            )
        | ValueNone ->
            ()

    let rectangle (thickness:int) (color:Color) (start:Vector2) (stop:Vector2) =
        let rect = Rectangle.fromVectors start stop
        Raylib.DrawRectangleRec(rect, Raylib.ColorAlpha(color, 0.1f))
        Raylib.DrawRectangleLinesEx(rect, float32 thickness, color)
