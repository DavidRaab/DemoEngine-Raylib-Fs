module MyGame.App
open Raylib_cs
open System.Numerics
open MyGame
open Storage
open Model

// Some global variables
let mutable assets : Assets = Unchecked.defaultof<_>

// Called in initModel - Sets up all the boxes
let boxes () =
    // black box that rotates
    let boxesOrigin = Entity.init (fun e ->
        e |> Entity.addView Layer.BG1 ({
            Comp.createViewfromSprite Center assets.Sprites.WhiteBox
            with Tint = Color.Black
        })
        e |> Entity.addTransform (Comp.createTransformXY 0f 0f)
        e |> Entity.addMovement {
            Direction = ValueNone // ValueSome (Relative (Vector2.Right * 50f))
            Rotation  = ValueSome 90f<deg>
        }
    )

    let boxes = ResizeArray<_>()
    // Changed the test so half ob boxes are local and the other half has
    // a parent. This is also a good test as updating the transform has to
    // check if something is local or parent. So more realistic workload for a
    // later game. Also removed testing just 3000 or 6000 boxes as framerates
    // are anyway too high for this less boxes.
    //
    //                 All     | Culling (boxes visible)
    //                ---------+----------
    // 10000 boxes ->  680 fps | 1500 fps (3000+)
    // 40000 boxes ->  180 fps |  800 fps (5000+)
    // 90000 boxes ->   75 fps |  600 fps (5000+)
    //
    let mutable makeParent = true
    for x=1 to 100 do
        for y=1 to 100 do
            boxes.Add (Entity.init (fun box ->
                let t =
                    if makeParent then
                        // this cost a lot of performance because rotation/position/scale of all 3.000 boxes
                        // must be computed with a matrix calculated of the parent.
                        makeParent <- not makeParent
                        Comp.createTransformXY (float32 x * 11f) (float32 y * 11f)
                        |>Comp.addTransformParent boxesOrigin
                    else
                        makeParent <- not makeParent
                        Comp.createTransformXY (float32 x * 11f) (float32 y * 11f)

                box |> Entity.addTransform t
                box |> Entity.addView Layer.BG2 (Comp.createViewFromSheets Center assets.Box)
                box |> Entity.addAnimation (Comp.createAnimationFromSheets assets.Box)
                box |> Entity.addMovement {
                    Direction = ValueNone
                    Rotation  = ValueSome (90f<deg>)
                }
            ))

    // let all boxes move
    // With 40,000 boxes it caused stutter. Because every second i iterated through
    // all 40,000 boxes and gave them a new direction and this takes some time. So
    // instead of updating all i just update some boxes every call. So work is split
    // across several frames.
    // Another option would be to put the work on another Thread that don't block
    // the main Thread. But some Stutter may also be caused by GC. As Movement is
    // an immutable data-structure at the moment and updating all means it creates
    // 40,000 objects immediately when it runs. But anyway with changed implementation
    // i couldn't notice any stutter. But also could anyway change it to a mutable
    // structure if needed.
    Systems.Timer.addTimer (Timer.every (sec 0.1) 0 (fun idx dt ->
        // changes direction and rotation of 200 boxes every call to a new random direction/rotation
        let updatesPerCall = boxes.Count / 10
        let last           = (boxes.Count - 1)
        let max            = if idx+updatesPerCall > last then last else idx+updatesPerCall
        for i=idx to max do
            // 10% of all boxes will move to world position 0,0 with 10px per second
            // all other boxes move in a random direction at 25px per second
            let box = boxes.[i]
            box |> Entity.addMovement {
                Direction = ValueSome(
                    if   State.rng.NextSingle() < 0.1f
                    then Absolute (Vector2.Zero, 25f)
                    else Relative (Vector2(randf -1f 1f, randf -1f 1f) * 25f)
                )
                Rotation = ValueSome(randf -30f 30f * 1f<deg>)
            }
        if max = last
        then State 0
        else State max
    ))

    boxes


// Initialize the Game Model
let initModel () =
    let boxes = boxes ()

    let arrow = Entity.init (fun e ->
        e |> Entity.addTransform (
            Comp.createTransformXY 100f 100f
            // |> Transform.setRotationVector (Vector2.Right)
        )
        e |> Entity.addView Layer.FG1 (Comp.createViewfromSprite Center assets.Sprites.Arrow)
        Systems.Timer.addTimer (Timer.every (sec 0.1) () (fun _ dt ->
            match Entity.getTransform e with
            | ValueSome t -> t.Rotation <- t.Rotation + 10f<deg>
            | ValueNone   -> ()
            State ()
        ))
    )

    let knight = Entity.init (fun e ->
        e |> Entity.addTransform (Comp.createTransformXY 0f 0f)
        e |> Entity.addView Layer.FG1 ({
            Comp.createViewFromSheets Top assets.Knight
            with Scale = (Vector2.create 2f 2f)
        })
        e |> Entity.addAnimation (Comp.createAnimationFromSheets assets.Knight)
    )

    // Creates a box that is a parent of the knight and moves when Knight moves
    let box = Entity.init (fun e ->
        e |> Entity.addTransform (
            Comp.createTransformXY 0f 80f
            |> Comp.addTransformParent knight
        )
        e |> Entity.addView Layer.FG1 ({
            Comp.createViewfromSprite Center assets.Sprites.WhiteBox
            with Tint = Color.Blue
        })
    )

    let sun = Entity.init (fun e ->
        e |> Entity.addTransform (Comp.createTransformXY 200f 200f)
        e |> Entity.addView Layer.FG1 ({
            Comp.createViewfromSprite Center assets.Sprites.WhiteBox
            with Tint = Color.Yellow
        })
        Systems.Timer.addTimer (Timer.every (sec 0.1) (Choice1Of2 0) (fun state dt ->
            match state with
            | Choice1Of2 right ->
                Entity.getTransform e
                |> ValueOption.iter (fun t -> t.Position <-  (t.Position + (Vector2.Right * 5f)))

                if right < 20
                then State (Choice1Of2 (right+1))
                else State (Choice2Of2 (right-1))
            | Choice2Of2 left ->
                Entity.getTransform e
                |> ValueOption.iter (fun t -> t.Position <- (t.Position + (Vector2.Left * 5f)))

                if left > 0
                then State (Choice2Of2 (left-1))
                else State (Choice1Of2 (left+1))
        ))
    )

    let planet1 = Entity.init (fun e ->
        e |> Entity.addTransform(
            Comp.createTransformXY 0f -100f
            |> Comp.addTransformParent sun
        )
        e |> Entity.addView Layer.FG1 ({
            Comp.createViewfromSprite Center assets.Sprites.WhiteBox
            with Tint = Color.DarkGreen
        })
    )

    let planet2 = Entity.init (fun e ->
        e |> Entity.addTransform(
            Comp.createTransformXY 0f -50f
            |> Comp.addTransformParent planet1
        )
        e |> Entity.addView Layer.FG1 ({
            Comp.createViewfromSprite Center assets.Sprites.WhiteBox
            with Tint = Color.DarkPurple
        })
    )

    let planet3 = Entity.init (fun e ->
        e |> Entity.addTransform(
            Comp.createTransformXY 0f -20f
            |> Comp.addTransformParent planet2
        )
        e |> Entity.addView Layer.FG1 ({
            Comp.createViewfromSprite Center assets.Sprites.WhiteBox
            with Tint = Color.Brown
        })
    )

    // Let stars rotate at 60 fps and 1° each frame
    Systems.Timer.addTimer (Timer.every (sec (1.0/60.0)) () (fun _ _ ->
        [sun;planet1;planet2;planet3] |> List.iter (fun p ->
            match Entity.getTransform p with
            | ValueSome t -> t.Rotation <- (t.Rotation + 1f<deg>)
            | ValueNone   -> ()
        )
        State ()
    ))

    // Makes the box over the knight move from left/right like Knight Rider!
    Systems.Timer.addTimer (Timer.every (sec 0.1) (Choice1Of2 0) (fun state dt ->
        match state with
        | Choice1Of2 state ->
            match Entity.getTransform box with
            | ValueSome t -> t.Position <- t.Position + (Vector2.create 10f 0f)
            | ValueNone   -> ()

            if state < 4
            then State (Choice1Of2 (state+1))
            else State (Choice2Of2 (state+1))
        | Choice2Of2 state ->
            match Entity.getTransform box with
            | ValueSome t -> t.Position <- t.Position + (Vector2.create -10f 0f)
            | ValueNone   -> ()

            if state > -4
            then State (Choice2Of2 (state-1))
            else State (Choice1Of2 (state-1))
    ))

    // Periodically run Garbage Collector
    Systems.Timer.addTimer (Timer.every (sec 10.0) () (fun _ _ ->
        System.GC.Collect ()
        State ()
    ))

    let gameState = {
        Knight         = knight
        MouseRectangle = NoRectangle
        Boxes          = boxes
    }
    gameState

let mutable knightState = IsIdle

// A Fixed Update implementation that tuns at the specified fixedUpdateTiming
let mutable resetInput = false
let fixedUpdateTiming = 1.0f / 60.0f
let fixedUpdate model (deltaTime:float32) =
    Systems.Timer.update deltaTime
    Systems.Movement.update deltaTime
    Systems.Animations.update deltaTime
    Systems.Transform.update ()

    (** At the moment multi-threading basically has no improvement. Most work
        is done in Movement and Transform, and both of those cannot
        run in parallel because they both update Transforms. So the
        multi-threading code must process both of those in order. Only thing
        what async does is put computation onto another thread while main
        thread waits for finish.

        In order to really get performance improvement I need to make the
        systems itself run in parallel. Here is one advantage of an Array.
        On an array different Threads could update different portions of
        the array indexes. But with a Dictionary, what i use currently, this
        is not achiveable. Or i write my own Dictionary implementation with
        access to the internal array that a Dictionary also uses.
    *)
    (*
    let multi = async {
        // Start those in parallel
        let! timer = Async.StartChild (async {
            do! Async.SwitchToThreadPool ()
            Systems.Timer.update deltaTime
        })
        let! mov   = Async.StartChild (async {
            do! Async.SwitchToThreadPool ()
            Systems.Movement.update deltaTime
        })
        let! anim  = Async.StartChild (async {
            do! Async.SwitchToThreadPool ()
            Systems.Animations.update deltaTime
        })

        // wait for timer and mov to finish
        do! timer
        do! mov

        // then start transform
        let! trans = Async.StartChild (async {
            do! Async.SwitchToThreadPool ()
            Systems.Transform.update ()
        })

        // Wait for transform and animation to finish
        do! trans
        do! anim
    }
    Async.RunSynchronously multi
    *)

    model


let mutable fixedUpdateElapsedTime = 0f
let update (model:Model) (deltaTime:float32) =
    FPS.update deltaTime

    // Input Handling
    // Get all Input of user and maps them into actions
    let actions = Array.ofSeq (Input.getActions inputMapping)

    // Handle Rectangle Drawing
    let model =
        let mutable model = model
        for action in actions do
            match action with
            | DragStart start ->
                model <- { model with MouseRectangle = StartRectangle start }
            | DragBetween p ->
                let mr =
                    match model.MouseRectangle with
                    | NoRectangle             -> StartRectangle (p)
                    | StartRectangle start    -> DrawRectangle  (start,p)
                    | DrawRectangle (start,_) -> DrawRectangle  (start,p)
                    | EndRectangle  (_, _)    -> StartRectangle (p)
                model <- { model with MouseRectangle = mr }
            | DragEnd (stop) ->
                let mr =
                    match model.MouseRectangle with
                    | NoRectangle             -> NoRectangle
                    | StartRectangle start    -> NoRectangle
                    | DrawRectangle (start,_) -> EndRectangle (start,stop)
                    | EndRectangle  (_, _)    -> NoRectangle
                model <- { model with MouseRectangle = mr }
            | _ -> ()
        model


    // A state machine, but will be replaced later by some library
    let nextKnightState previousState =
        // helper-function that describes how an action is mapped to a knightState
        let action2state = function
            | Attack      -> IsAttack (0f, Comp.getSheetDuration (Entity.getSheetExn "Attack" model.Knight))
            | MoveLeft  v -> IsLeft v
            | MoveRight v -> IsRight v
            | Crouch      -> IsCrouch
            | Movement v  ->
                if   v.X > 0f then IsRight <| Vector2(v.X,0f)
                elif v.X < 0f then IsLeft  <| Vector2(v.X,0f)
                else IsIdle
            | _           -> IsIdle

        // helper-function that describes the transition to a new state. Mostly it means setting the
        // correct animation and moving the character
        let setState state =
            match state with
            | IsAttack (e,d) -> IsAttack (e,d)
            | IsCrouch       -> IsCrouch
            | IsLeft v       ->
                // model.Knight |> State.View.iter (View.flipHorizontal true)
                Entity.getTransform model.Knight
                |> ValueOption.iter (fun t -> t.Position <- t.Position + (v * 300f * deltaTime))
                IsLeft v
            | IsRight v     ->
                // model.Knight |> State.View.iter      (View.flipHorizontal false)
                Entity.getTransform model.Knight
                |> ValueOption.iter (fun t -> t.Position <- t.Position + (v * 300f * deltaTime))
                IsRight v
            | IsIdle -> IsIdle

        let setAnimation state =
            let anim =
                match state with
                | IsAttack (_,_) -> "Attack"
                | IsCrouch       -> "Crouch"
                | IsLeft _       -> "Run"
                | IsRight _      -> "Run"
                | IsIdle         -> "Idle"
            Entity.setAnimation anim model.Knight

        // 1. Find the next state by mapping every action to a state, and get the one with the highest priority.
        //    For example, when user hits Attack button, it has higher priority as moving
        let wantedState =
            match Array.map action2state actions with
            | [||] -> IsIdle
            | xs   -> Array.maxBy statePriority xs

        // 2. Real state machine. Checks the current state, and the new state, and does
        //    a transition to the new state if allowed.
        match previousState, wantedState with
        | IsAttack (e,d), wantedState ->
            let elapsed = e + deltaTime
            if elapsed >= d
            then
                setAnimation wantedState
                setState wantedState
            else IsAttack (elapsed,d)
        | previous, wanted  ->
            // When state changed we need to switch animation
            if previous <> wanted then
                setAnimation wanted
            setState wanted

    // Compute new Knight State
    knightState <- nextKnightState knightState

    // Update Camera
    let inline setCameraZoom value =
        State.camera.Zoom <- clampF 0.1f 3f value

    for action in actions do
        match action with
        | CameraHome                  -> State.camera.Target <- (Vector2.create 0f 0f)
        | ZoomReset                   -> State.camera.Zoom   <- 1f
        | ZoomIn                      -> setCameraZoom (State.camera.Zoom + (1f * deltaTime))
        | ZoomOut                     -> setCameraZoom (State.camera.Zoom - (1f * deltaTime))
        | ScrollZoom (IsGreater 0f x) -> setCameraZoom (State.camera.Zoom + 0.1f)
        | ScrollZoom (IsSmaller 0f x) -> setCameraZoom (State.camera.Zoom - 0.1f)
        | Camera v                    ->
            let newPos =
                State.camera.Target
                + ( (v * 300f * deltaTime) * (1f / State.camera.Zoom) )
            State.camera.Target <- newPos
        | _                           -> ()

    // FixedUpdate Handling
    fixedUpdateElapsedTime <- fixedUpdateElapsedTime + deltaTime
    let model =
        if fixedUpdateElapsedTime >= fixedUpdateTiming then
            fixedUpdateElapsedTime <- fixedUpdateElapsedTime - fixedUpdateTiming
            fixedUpdate model fixedUpdateTiming
        else
            model

    (*
    // Vibration through Triggers
    // printfn "%f %f" gamePad.Triggers.Left gamePad.Triggers.Right
    GamePad.SetVibration(0,
        gamePad.Triggers.Left,
        gamePad.Triggers.Right
    ) |> ignore

    if keyboard.IsKeyDown Keys.Space then
        ignore <| GamePad.SetVibration(0, 1.0f, 1.0f)

    if GamePad.isPressed gamePad.Buttons.A then
        printfn "Pressed A"

    if GamePad.isPressed gamePad.Buttons.Back || keyboard.IsKeyDown Keys.Escape then
        game.Exit()
    *)

    model

// Some begin/end helper functions
let inline beginTextureMode target ([<InlineIfLambda>] f) =
    Raylib.BeginTextureMode(target)
    f ()
    Raylib.EndTextureMode()

let inline beginMode2D camera ([<InlineIfLambda>] f) =
    Raylib.BeginMode2D(camera)
    f ()
    Raylib.EndMode2D ()

let inline beginDrawing ([<InlineIfLambda>] f) =
    Raylib.BeginDrawing ()
    f ()
    Raylib.EndDrawing ()

// Those are the variables used for rendering into RenderingTexture
// They are initialized on program start.
let mutable target     = Unchecked.defaultof<RenderTexture2D>
let mutable sourceRect = Unchecked.defaultof<Rectangle>
let mutable destRect   = Unchecked.defaultof<Rectangle>

let drawUI model =
    Gui.nextFrame ()
    FPS.draw ()

    let mousePos = Raylib.GetMousePosition()
    Systems.Drawing.mousePosition  (mousePos) 20 (Vector2.create 0f 320f)
    Systems.Drawing.trackPosition  model.Knight 20 (Vector2.create 0f 340f)

    let mutable visibleCount = 0
    for layer in State.View.Data.Keys do
        visibleCount <- visibleCount + Storage.length State.View.Data.[layer]
    Raylib.DrawText(
        text     = String.Format("Visible: {0} {1}", visibleCount, State.drawed),
        posX     = 250,
        posY     = 3,
        fontSize = 20,
        color    = Color.Yellow
    )

    let inline rect x y w h =
        Rectangle(x,y,w,h)

    if Gui.button (rect 510f 330f 50f 24f) "Spawn" then
        for i=1 to 100 do
            Entity.init (fun e ->
                let x,y =
                    State.rng.NextSingle() * 2000f - 1000f,
                    State.rng.NextSingle() * 2000f - 1000f
                e |> Entity.addTransform (Comp.createTransformXY x y)
                e |> Entity.addView Layer.BG1 (Comp.createViewFromSheets Center assets.Box)
                model.Boxes.Add(e)
            ) |> ignore

    if Gui.button (rect 570f 330f 60f 24f) "Destroy" then
        for i=1 to 100 do
            if model.Boxes.Count > 0 then
                let idx = State.rng.Next(model.Boxes.Count)
                let e = model.Boxes.[idx]
                model.Boxes.RemoveAt(idx)
                Entity.destroy e

let draw (model:Model) (deltaTime:float32) =
    beginTextureMode target (fun () ->
        Raylib.ClearBackground(Color.DarkBlue)

        // Draw GameObjects
        beginMode2D State.camera (fun () ->
            Systems.View.draw ()

            // Vector Position should be World Positions, except in
            // DrawRectangle. stop still contains a Screen Position
            match model.MouseRectangle with
            | NoRectangle         -> ()
            | StartRectangle _    -> ()
            | DrawRectangle (start,stop) ->
                let stop = Raylib.GetScreenToWorld2D(stop,State.camera)
                Systems.Drawing.rectangle 2 Color.Black start stop
            | EndRectangle (start,stop) ->
                Systems.Drawing.rectangle 2 Color.Black start stop
        )

        // Draw UI
        beginMode2D State.uiCamera (fun () ->
            drawUI model
        )
    )

    beginDrawing (fun () ->
        Raylib.ClearBackground(Color.Black)

        // Draw RenderTexture
        beginMode2D State.uiCamera (fun () ->
            Raylib.DrawTexturePro(target.Texture, sourceRect, destRect, Vector2(0f,0f), 0f, Color.White)
        )
    )

[<EntryPoint;System.STAThread>]
let main argv =
    // The Game uses a virtual Render solution. It renders everything to a
    // RenderTexture with that Resolution. Then this RenderTexture is scaled to
    // the window screen. Scaling tries to fit as much of the windows as it is
    // possible while keeping aspect Ratio of the defined virtual resolution intact.
    let screenWidth,  screenHeight  = 1280, 720
    let virtualWidth, virtualHeight = 640, 360
    let screenAspect = float32 screenWidth  / float32 screenHeight
    let targetAspect = float32 virtualWidth / float32 virtualHeight

    // this calculates the real resolution the game uses in the window
    let width,height =
        if targetAspect <= screenAspect then
            let h = float32 screenHeight
            let w = h * targetAspect
            w,h
        else
            let w = float32 screenWidth
            let h = w / targetAspect
            w,h

    Raylib.InitWindow(screenWidth,screenHeight,"Raylib Demo")
    Raylib.SetMouseCursor(MouseCursor.Crosshair)
    // We need to set a Mouse Scale so we don't get the screen position, we instead get
    // a position that is conform with our virtual Resolution. When a virtual resolution
    // of 640 x 360 is defined then GetMousePosition() will also return 640 x 360
    // when mouse cursor is in bottomRight position independent of the real window size.
    Raylib.SetMouseScale((float32 virtualWidth / width), (float32 virtualHeight / height))

    // initialize RenderTexture
    target     <- Raylib.LoadRenderTexture(virtualWidth, virtualHeight)
    sourceRect <- Rectangle(0f, 0f, float32 target.Texture.Width, float32 -target.Texture.Height)
    destRect   <- Rectangle(0f, 0f, width, height)

    // Initialize Cameras
    let offset = Vector2(float32 virtualWidth / 2f, float32 virtualHeight /2f)
    State.camera   <- Camera2D(offset,       Vector2.Zero, 0f, 1f) // World Camera
    State.uiCamera <- Camera2D(Vector2.Zero, Vector2.Zero, 0f, 1f) // Camera for GUI elements

    // Set Object Culling Properties
    Systems.View.offset <- 64f
    Systems.View.halfX  <- float32 virtualWidth  / 2f
    Systems.View.halfY  <- float32 virtualHeight / 2f

    // Load Game Assets and initialize first Model
    assets  <- Assets.load ()
    let mutable model = initModel ()

    for i=0 to 3 do
        if CBool.op_Implicit (Raylib.IsGamepadAvailable(i)) then
            printfn "INFO: GamePad %d Available" i
        else
            printfn "INFO: No GamePad %d" i

    // Game Loop
    while not (CBool.op_Implicit (Raylib.WindowShouldClose())) do
        let deltaTime = Raylib.GetFrameTime ()
        model <- update model deltaTime
        draw model deltaTime

    // TODO: Proper Unloading of resources
    Raylib.UnloadRenderTexture(target)
    Raylib.CloseWindow()

    1
