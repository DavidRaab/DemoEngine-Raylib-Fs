module MyGame.Systems
open Raylib_cs
open System.Numerics
open Storage
open Sto2

type Transform = MyGame.Transform
type Parallel  = System.Threading.Tasks.Parallel

// Similar to Parrallel.For that I used before. But profiling showed a lot of
// garbage and thread waiting. Don't know how it exactly works, so just rewritten
// it myself. It splits the work across a fixed amount of threads, what usually
// is anyway better in a game to have a fixed amount of workers. Than the main
// thread basically spinlocks until all threads finish. This way i still can use
// a loop to process multiple items instead of calling a function for every
// element. This reduces overhead. F# "inline" feature here is important to
// eleminate function calls. I guess the .Net Parallel.For maybe don't have
// this feature, or maybe JIT inlining does it. Who knows?
let inline runThreaded threadAmount count ([<InlineIfLambda>] f) =
    let workPerThread          = count / threadAmount
    let mutable runningThreads = 4

    for currentThread=0 to threadAmount-1 do
        let start = currentThread * workPerThread
        let stop  =
            if currentThread < (threadAmount-1)
            then ((currentThread+1) * workPerThread) - 1
            else count - 1

        ignore <| System.Threading.ThreadPool.QueueUserWorkItem(fun _ ->
            for i=start to stop do
                f i
            System.Threading.Interlocked.Decrement(&runningThreads) |> ignore
        )

    while runningThreads > 0 do
        ()

// Transform System updates the Global_ fields when a Parent is set
module Transform =
    // Calculates position, rotation and scale relative to parent
    // returns an voption because it is called recursively on transform. ValueNone
    // indicates when a parent has no transform defined and recursion ends.
    let rec calculateTransform (me:Transform) =
        match me.Parent with
        | ValueNone        -> ValueSome (struct (me.Position,me.Rotation,me.Scale))
        | ValueSome parent ->
            match Storage.get parent State.Transform with
            | ValueSome parent ->
                match calculateTransform parent with
                | ValueNone                    -> ValueNone
                | ValueSome (pPos,pRot,pScale) ->
                    let scale = Vector2.create (pScale.X * me.Scale.X) (pScale.Y * me.Scale.Y)
                    let pos   = Vector2.Transform(
                        me.Position,
                        Matrix.CreateScale(scale.X, scale.Y, 0f)
                        * Matrix.CreateRotationZ(float32 (Rad.fromDeg pRot)) // rotate by parent position
                        * Matrix.CreateTranslation(pPos.X, pPos.Y, 0f)       // translate by parent position
                    )
                    ValueSome (struct (pos,pRot+me.Rotation,scale))
            | ValueNone -> ValueNone

    let inline updateIndex idx =
        // Get a Transform
        let struct (_,t) = State.Transform.Data.[idx]
        match calculateTransform t with
        | ValueNone -> ()
        | ValueSome (pos,rot,scale) ->
            t.GlobalPosition.X <- pos.X
            t.GlobalPosition.Y <- pos.Y
            t.GlobalRotation   <- rot
            t.GlobalScale.X    <- scale.X
            t.GlobalScale.Y    <- scale.Y

    /// Updates all Global fields of every Transform with a Parent
    let update () =
        Parallel.For(0, State.Transform.Data.Count, updateIndex)
        |> ignore

// View System draws entity
module View =
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

    let inline drawTexture (transform:Transform) view =
        let pos   = transform.GlobalPosition
        let rot   = transform.GlobalRotation
        let scale = transform.GlobalScale
        if pos.X > minX && pos.X < maxX && pos.Y > minY && pos.Y < maxY then
            State.drawed <- State.drawed + 1
            Raylib.DrawTexturePro(
                texture  = view.Sprite.Texture,
                source   = view.Sprite.SrcRect,
                dest     = Rectangle(pos, view.Sprite.SrcRect.Width * scale.X * view.Scale.X, view.Sprite.SrcRect.Height * scale.Y * view.Scale.Y),
                origin   = view.Origin,
                rotation = float32 (rot + view.Rotation),
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

        State.View |> Sto2.iter Layer.BG3 (fun entity v ->
            match Entity.getTransform entity with
            | ValueSome t -> drawTexture t v
            | ValueNone   -> ()
        )

        State.View |> Sto2.iter Layer.BG2 (fun entity v ->
            match Entity.getTransform entity with
            | ValueSome t -> drawTexture t v
            | ValueNone   -> ()
        )

        State.View |> Sto2.iter Layer.BG1 (fun entity v ->
            match Entity.getTransform entity with
            | ValueSome t -> drawTexture t v
            | ValueNone   -> ()
        )

        State.View |> Sto2.iter Layer.FG3 (fun entity v ->
            match Entity.getTransform entity with
            | ValueSome t -> drawTexture t v
            | ValueNone   -> ()
        )

        State.View |> Sto2.iter Layer.FG2 (fun entity v ->
            match Entity.getTransform entity with
            | ValueSome t -> drawTexture t v
            | ValueNone   -> ()
        )

        State.View |> Sto2.iter Layer.FG1 (fun entity v ->
            match Entity.getTransform entity with
            | ValueSome t -> drawTexture t v
            | ValueNone   -> ()
        )

module AutoMovement =
    let update (dt:float32) =
        for idx=0 to State.AutoMovement.Data.Count-1 do
            let struct (entity,mov) = State.AutoMovement.Data.[idx]
            match Storage.get entity State.Transform with
            | ValueSome t ->
                t.Position <- t.Position + (mov.Direction * dt)
                t.Rotation <- t.Rotation + (mov.RotateBy  * dt)
            | ValueNone   -> ()

module AutoTargetPosition =
    let update (dt:float32) =
        for idx=0 to State.AutoTargetPosition.Data.Count-1 do
            let struct (entity,mov) = State.AutoTargetPosition.Data.[idx]
            match Storage.get entity State.Transform with
            | ValueSome t ->
                let direction = Vector2.Normalize(mov.Position - t.Position)
                t.Position <- t.Position + direction * mov.Speed * dt
            | ValueNone -> ()

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
    let inline updateAnimation dt idx =
        let struct (entity, anim) = State.Animation.Data.[idx]
        anim.ElapsedTime <- anim.ElapsedTime + dt
        if anim.ElapsedTime > anim.CurrentSheet.FrameDuration then
            anim.ElapsedTime <- anim.ElapsedTime - anim.CurrentSheet.FrameDuration
            Comp.setAnimationNextSprite anim
            match Sto2.get entity State.View with
            | ValueSome (_,view) ->
                match Comp.getCurrentSpriteAnimation anim with
                | ValueSome sprite -> view.Sprite <- sprite
                | ValueNone        -> ()
            | ValueNone          -> ()

    let update (deltaTime:float32) =
        Parallel.For(0, State.Animation.Data.Count, updateAnimation deltaTime)
        |> ignore

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
        match Entity.getTransform entity with
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
