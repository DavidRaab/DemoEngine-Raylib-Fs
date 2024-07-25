namespace MyGame.Utility
open Raylib_cs
open System.Numerics
open MyGame.Extensions
open MyGame.DataTypes
open MyGame.Components

type String   = System.String
type TimeSpan = System.TimeSpan
type Key      = KeyboardKey

type FKeyboardState() =
    let state = System.Collections.BitArray(512)
    member this.SetKey (key:Key) =
        state.Set(int key, true)
    member this.GetKey (key:Key) =
        state.Get(int key)
    member this.SetKeys () =
        let mutable key = Raylib.GetKeyPressed ()
        while key <> 0 do
            state.Set(key, true)
            key <- Raylib.GetKeyPressed()
    member this.IsKeyDown(key:Key) =
        state.Get(int key) = true
    member this.IsKeyUp(key:Key) =
        state.Get(int key) = false

module FKeyboard =
    let mutable previousState = FKeyboardState()
    let mutable currentState  = FKeyboardState()

    let addKeys () =
        currentState.SetKeys ()

    let nextState () =
        previousState <- currentState
        currentState  <- FKeyboardState()

    /// returns true only in the first frame the key was pressed.
    /// Key must be released again to become true again
    let isPressed key =
        previousState.IsKeyUp key && currentState.IsKeyDown key

    /// returns true only in the first frame the key was released
    /// Key must be pressed again to become true again
    let isReleased key =
        previousState.IsKeyDown key && currentState.IsKeyUp key

    let isKeyDown key =
        currentState.GetKey key

    let isKeyUp key =
        currentState.IsKeyUp key

type FGamePadButton =
    | A             = 0
    | B             = 1
    | X             = 2
    | Y             = 3
    | LeftShoulder  = 4
    | RightShoulder = 5
    | LeftStick     = 6
    | RightStick    = 7
    | DPadLeft      = 8
    | DPadUp        = 9
    | DPadRight     = 10
    | DPadDown      = 11
    | Back          = 12
    | BigButton     = 13
    | Start         = 14

type FGamePadState() =
    let state                = System.Collections.BitArray(15)
    let mutable triggerLeft  = 0f
    let mutable triggerRight = 0f
    let mutable stickLeft    = Vector2.Zero
    let mutable stickRight   = Vector2.Zero

    member this.SetButton (button:FGamePadButton) =
        state.Set(int button, true)
    member this.GetButton (button:FGamePadButton) =
        state.Get(int button)
    member this.AddGamePadState () =
        // Map MonoGame Buttons to own State
        let buttonMapping buttons =
            for (mb:GamepadButton,fb) in buttons do
                if CBool.op_Implicit(Raylib.IsGamepadButtonDown(0, mb)) then this.SetButton(fb)

        buttonMapping [
            GamepadButton.RightFaceDown,  FGamePadButton.A
            GamepadButton.RightFaceRight, FGamePadButton.B
            GamepadButton.RightFaceLeft,  FGamePadButton.X
            GamepadButton.RightFaceUp,    FGamePadButton.Y
            GamepadButton.LeftTrigger1,   FGamePadButton.LeftShoulder
            GamepadButton.RightTrigger1,  FGamePadButton.RightShoulder
            GamepadButton.LeftTrigger1,   FGamePadButton.LeftStick
            GamepadButton.RightTrigger2,  FGamePadButton.RightStick
            GamepadButton.MiddleLeft,     FGamePadButton.Back
            GamepadButton.Middle,         FGamePadButton.BigButton
            GamepadButton.MiddleRight,    FGamePadButton.Start
            GamepadButton.LeftFaceDown,   FGamePadButton.DPadLeft
            GamepadButton.LeftFaceUp,     FGamePadButton.DPadUp
            GamepadButton.LeftFaceRight,  FGamePadButton.DPadRight
            GamepadButton.LeftFaceLeft,   FGamePadButton.DPadRight
        ]

        // Keep Track of the highest value
        triggerLeft  <- max triggerLeft  (Raylib.GetGamepadAxisMovement(0, GamepadAxis.LeftTrigger))
        triggerRight <- max triggerRight (Raylib.GetGamepadAxisMovement(0, GamepadAxis.RightTrigger))

        // Save last Thumbstick values
        stickLeft  <- Vector2(
            Raylib.GetGamepadAxisMovement(0, GamepadAxis.LeftX),
            Raylib.GetGamepadAxisMovement(0, GamepadAxis.LeftY)
        )
        stickRight <- Vector2(
            Raylib.GetGamepadAxisMovement(0, GamepadAxis.RightX),
            Raylib.GetGamepadAxisMovement(0, GamepadAxis.RightY)
        )

    member this.TriggerLeft  = triggerLeft
    member this.TriggerRight = triggerRight
    member this.StickLeft    = stickLeft
    member this.StickRight   = stickRight

    member this.IsKeyUp button =
        state.Get(int button) = false
    member this.IsKeyDown button =
        state.Get(int button) = true

module FGamePad =
    let mutable previousState = FGamePadState()
    let mutable currentState  = FGamePadState()

    let addState () =
        currentState.AddGamePadState ()

    let nextState () =
        previousState <- currentState
        currentState  <- FGamePadState()

    /// `true` only in the exact frame this button is pressed
    let isPressed button =
        previousState.IsKeyUp button && currentState.IsKeyDown button

    /// `true` only in the frame the button is released
    let isReleased button =
        previousState.IsKeyDown button && currentState.IsKeyUp button

    let isKeyDown button =
        currentState.IsKeyDown button

    let isKeyUp button =
        currentState.IsKeyUp button

    let stickLeft    () = currentState.StickLeft
    let stickRight   () = currentState.StickRight
    let triggerLeft  () = currentState.TriggerLeft
    let triggerRight () = currentState.TriggerRight

type MouseButton =
    | Left     = 0
    | Middle   = 1
    | Right    = 2
    | XButton1 = 3
    | XButton2 = 4

type FMouseState() =
    let state                         = System.Collections.BitArray(5)
    let mutable position              = Vector2(0f,0f)
    let mutable scrollWheel           = 0f

    member this.SetButton(button:MouseButton) =
        state.Set(int button,true)
    member this.GetButton(button:MouseButton) =
        state.Get(int button)
    member this.AddMouseState () =
        // Map Mouse Button to BitArray
        let mapButtons buttons =
            for (mb,fb) in buttons do
                if CBool.op_Implicit <| Raylib.IsMouseButtonDown(mb) then
                    this.SetButton(fb)
        mapButtons [
            Raylib_cs.MouseButton.Left,   MouseButton.Left
            Raylib_cs.MouseButton.Middle, MouseButton.Middle
            Raylib_cs.MouseButton.Right,  MouseButton.Right
            Raylib_cs.MouseButton.Back,   MouseButton.XButton1
            Raylib_cs.MouseButton.Extra,  MouseButton.XButton2
        ]

        // Apply Viewport Offset or otherwise everything is fucked up
        // position              <- Point(ms.X - camera.Viewport.X, ms.Y - camera.Viewport.Y)
        position              <- Raylib.GetMousePosition()
        scrollWheel           <- Raylib.GetMouseWheelMove()

    member this.Position = position
    member this.ScrollWheel
        with get () = scrollWheel
        and  set  x = scrollWheel <- x

    member this.IsKeyDown button =
        this.GetButton(button) = true
    member this.IsKeyUp button =
        this.GetButton(button) = false

module FMouse =
    let mutable previousState = FMouseState()
    let mutable currentState  = FMouseState()

    let addState mouseState =
        currentState.AddMouseState ()

    let nextState () =
        previousState                      <- currentState
        currentState                       <- FMouseState ()
        currentState.ScrollWheel           <- previousState.ScrollWheel

    /// `true` only in the exact frame the button was pressed
    let isPressed button =
        previousState.IsKeyUp button && currentState.IsKeyDown button

    /// `true` only in the exact frame the button is released
    let isReleased button =
        previousState.IsKeyDown button && currentState.IsKeyUp button

    let isKeyDown button =
        currentState.IsKeyDown button

    let isKeyUp button =
        currentState.IsKeyUp button

    let position    () : Vector2 = currentState.Position
    let scrollWheel () : float32 = currentState.ScrollWheel - previousState.ScrollWheel

// Input Module
type ButtonState =
    | IsPressed
    | IsReleased
    | IsKeyDown
    | IsKeyUp

type GamePadThumbStick<'Action> = {
    Left:  option<Vector2 -> 'Action>
    Right: option<Vector2 -> 'Action>
}

type GamePadTriggers<'Action> = {
    Left:  option<float32 -> 'Action>
    Right: option<float32 -> 'Action>
}

type InputGamePad<'Action> = {
    Buttons:    list<FGamePadButton * ButtonState * 'Action>
    ThumbStick: GamePadThumbStick<'Action>
    Trigger:    GamePadTriggers<'Action>
}

type FMouseAction<'Action> =
    | Screen of (Vector2 -> 'Action)
    | World  of (Vector2 -> 'Action)

type InputMouse<'Action> = {
    Buttons:               list<MouseButton * ButtonState * FMouseAction<'Action>>
    ScrollWheel:           option<float32 -> 'Action>
    Position:              option<Vector2 -> 'Action>
}

type Input<'Action> = {
    Keyboard:   list<Key * ButtonState * 'Action>
    GamePad:    InputGamePad<'Action>
    Mouse:      InputMouse<'Action>
}

module FInput =
    let mapInput camera definition =
        let actions = ResizeArray<_>()

        // Keyboard Input Handling
        for button,state,action in definition.Keyboard do
            match state with
            | IsPressed  -> if FKeyboard.isPressed  button then actions.Add action
            | IsReleased -> if FKeyboard.isReleased button then actions.Add action
            | IsKeyDown  -> if FKeyboard.isKeyDown  button then actions.Add action
            | IsKeyUp    -> if FKeyboard.isKeyUp    button then actions.Add action

        // GamePad Buttons Handling
        for button,state,action in definition.GamePad.Buttons do
            match state with
            | IsPressed  -> if FGamePad.isPressed  button then actions.Add action
            | IsReleased -> if FGamePad.isReleased button then actions.Add action
            | IsKeyDown  -> if FGamePad.isKeyDown  button then actions.Add action
            | IsKeyUp    -> if FGamePad.isKeyUp    button then actions.Add action

        // GamePad ThumbStick Handling
        if FGamePad.stickLeft () <> Vector2.Zero then
            definition.GamePad.ThumbStick.Left |> Option.iter (fun f ->
                actions.Add (f (Vector2.flipY (FGamePad.stickLeft ())))
            )
        if FGamePad.stickRight () <> Vector2.Zero then
            definition.GamePad.ThumbStick.Right |> Option.iter (fun f ->
                actions.Add (f (Vector2.flipY (FGamePad.stickRight ())))
            )

        // GamePad Triggers
        if FGamePad.triggerLeft () |> notNearly 0.0f 0.0001f then
            definition.GamePad.Trigger.Left |> Option.iter (fun f ->
                actions.Add (f (FGamePad.triggerLeft ()))
            )
        if FGamePad.triggerRight () |> notNearly 0.0f 0.0001f then
            definition.GamePad.Trigger.Right |> Option.iter (fun f ->
                actions.Add (f (FGamePad.triggerRight ()))
            )

        // Mouse Handling
        let mouseAction action =
            match action with
            | Screen f -> f (FMouse.position ())
            | World  f -> f (Camera.screenToWorld (FMouse.position ()) camera)

        for button,state,action in definition.Mouse.Buttons do
            match state with
            | IsPressed  -> if FMouse.isPressed  button then actions.Add (mouseAction action)
            | IsReleased -> if FMouse.isReleased button then actions.Add (mouseAction action)
            | IsKeyUp    -> if FMouse.isKeyUp    button then actions.Add (mouseAction action)
            | IsKeyDown  -> if FMouse.isKeyDown  button then actions.Add (mouseAction action)

        definition.Mouse.ScrollWheel |> Option.iter (fun f ->
            actions.Add (f (FMouse.scrollWheel ()))
        )
        definition.Mouse.Position |> Option.iter (fun f ->
            actions.Add (f (FMouse.position ()))
        )

        List.ofSeq actions

type FPS = {
    mutable Updates:     int
    mutable Draws:       int
    mutable ElapsedTime: TimeSpan
    mutable UpdateFPS:   float
    mutable DrawFPS:     float
}

module FPS =
    let create (fps:FPS) : FPS = fps

    // Global State
    let state = create {
        Updates     = 0
        Draws       = 0
        ElapsedTime = TimeSpan.Zero
        UpdateFPS   = 0
        DrawFPS     = 0
    }

    // Called on each update
    let update (deltaTime:TimeSpan) =
        state.Updates     <- state.Updates + 1
        state.ElapsedTime <- state.ElapsedTime + deltaTime

        if state.ElapsedTime >= TimeSpan.oneSecond then
            state.UpdateFPS   <- float state.Updates / state.ElapsedTime.TotalSeconds
            state.DrawFPS     <- float state.Draws   / state.ElapsedTime.TotalSeconds
            state.Updates     <- 0
            state.Draws       <- 0
            state.ElapsedTime <- TimeSpan.Zero

    let draw () =
        state.Draws <- state.Draws + 1
        Raylib.DrawText(
            text     = String.Format("Update/Draw: {0:0} {1:0}", state.UpdateFPS, state.DrawFPS),
            posX     = 3,
            posY     = 3,
            fontSize = 20,
            color    = Color.Yellow
        )

