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
        State.AutoRotation |> Storage.insert e {
            RotateBy = 90f<deg>
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

                State.AutoRotation |> Storage.insert box {
                    RotateBy = 90f<deg>
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
    let centerPosition = { Position = vec2 0f 0f; Speed = 25f }
    Systems.Timer.addTimer (Timer.every (sec 0.1) 0 (fun idx dt ->
        let updatesPerCall = 1000 //boxes.Count / 10
        let last           = (boxes.Count - 1)
        let max            = if idx+updatesPerCall > last then last else idx+updatesPerCall
        for i=idx to max do
            let box = boxes.[i]

            // 10% of all boxes will move to world position 0,0
            if State.rng.NextSingle() < 0.1f then
                match Storage.get box State.AutoTargetPosition with
                | ValueSome _ -> ()
                | ValueNone   ->
                    State.AutoMovement       |> Storage.remove box
                    State.AutoTargetPosition |> Storage.insert box centerPosition
            else
                match Storage.get box State.AutoMovement with
                | ValueSome dir -> dir.Direction <- vec2 (randf -1f 1f) (randf -1f 1f) * 25f
                | ValueNone     ->
                    State.AutoTargetPosition |> Storage.remove box
                    State.AutoMovement       |> Storage.insert box {
                        Direction = vec2 (randf -1f 1f) (randf -1f 1f) * 25f
                    }

            match Storage.get box State.AutoRotation with
            | ValueNone ->
                State.AutoRotation |> Storage.insert box {
                    RotateBy = (randf -30f 30f) * 1f<deg>
                }
            | ValueSome rot ->
                rot.RotateBy <- rot.RotateBy + (randf -30f 30f) * 1f<deg>

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
    // Systems.Timer.addTimer (Timer.every (sec 3.0) () (fun _ _ ->
    //     System.GC.Collect(0, System.GCCollectionMode.Optimized, false)
    //     State ()
    // ))

    let gameState = {
        Knight         = knight
        MouseRectangle = NoRectangle
        Boxes          = boxes
    }
    gameState

let mutable knightState = IsIdle

// A Fixed Update implementation that tuns at the specified fixedUpdateTiming
let fixedUpdateTiming = 1.0f / 60.0f
let fixedUpdate model (deltaTime:float32) =
    Systems.Timer.update deltaTime
    Async.RunSynchronously(async {
        let! d = Async.StartChild (async{ Systems.Animations.update deltaTime })
        let! a = Async.StartChild (async{ Systems.AutoMovement.update deltaTime })
        let! c = Async.StartChild (async{ Systems.AutoRotation.update deltaTime })
        let! b = Async.StartChild (async{ Systems.AutoTargetPosition.update deltaTime })
        do! a
        do! b
        do! c
        do! d
    })
    Systems.Transform.update ()
    model

let useFixedUpdate = true
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
    let model =
        if useFixedUpdate then
            fixedUpdateElapsedTime <- fixedUpdateElapsedTime + deltaTime
            if fixedUpdateElapsedTime >= fixedUpdateTiming then
                fixedUpdateElapsedTime <- fixedUpdateElapsedTime - fixedUpdateTiming
                fixedUpdate model fixedUpdateTiming
            else
                model
        else
            fixedUpdate model deltaTime

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
    Systems.Drawing.mousePosition      mousePos 8 (Vector2.create 0f 320f)
    Systems.Drawing.trackPosition  model.Knight 8 (Vector2.create 0f 340f)

    let mutable visibleCount = 0
    for layer in State.View.Data.Keys do
        visibleCount <- visibleCount + Storage.length State.View.Data.[layer]
    Raylib.DrawText(
        text     = String.Format("Visible: {0} {1}", visibleCount, State.drawed),
        posX     = 250,
        posY     = 3,
        fontSize = 8,
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

// or use: dotnet-counters monitor --process-id xxxx
// to print GC information on running process
let mutable gen0 = 0
let mutable gen1 = 0
let mutable gen2 = 0
let showGCInfo () =
    let printPause () =
        let mem  = float(System.GC.GetTotalMemory(false))         / 1024.0 / 1024.0
        let tacc = float(System.GC.GetTotalAllocatedBytes(false)) / 1024.0 / 1024.0
        let info = System.GC.GetGCMemoryInfo(System.GCKind.Any)
        let str  = System.String.Format("{0} {1} {2} Pause {3:0.00}% Mem {4:0.00} MiB Allocated {5:0.00} MiB",
            gen0, gen1, gen2,
            info.PauseTimePercentage,
            mem, tacc
        )
        System.Console.WriteLine(str)
    let g0 = System.GC.CollectionCount(0)
    let g1 = System.GC.CollectionCount(1)
    let g2 = System.GC.CollectionCount(2)
    if g0 <> gen0 then
        gen0 <- g0
        printPause ()
    if g1 <> gen1 then
        gen1 <- g1
        printPause ()
    if g2 <> gen2 then
        gen2 <- g2
        printPause ()

[<EntryPoint;System.STAThread>]
let main argv =
    printfn "Is GC ServerMode %b" System.Runtime.GCSettings.IsServerGC
    System.Runtime.GCSettings.LatencyMode <- System.Runtime.GCLatencyMode.SustainedLowLatency
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

    // Raylib.SetConfigFlags(ConfigFlags.VSyncHint ||| ConfigFlags.FullscreenMode )
    Raylib.InitWindow(screenWidth,screenHeight, "Raylib Demo")
    // Raylib.SetTargetFPS(75)
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
    Systems.View.offset <- 128f
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
        // showGCInfo ()
        let deltaTime = Raylib.GetFrameTime ()
        model <- update model deltaTime
        draw model deltaTime

    // TODO: Proper Unloading of resources
    Raylib.UnloadRenderTexture(target)
    Raylib.CloseWindow()

    1
