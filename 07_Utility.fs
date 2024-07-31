namespace MyGame.Utility
open Raylib_cs
open System.Numerics
open MyGame.State

type String   = System.String
type TimeSpan = System.TimeSpan

type Key     = KeyboardKey
type GamePad =
    | ButtonUp      = 0
    | ButtonRight   = 1
    | ButtonDown    = 2
    | ButtonLeft    = 3
    | LeftTrigger1  = 4
    | LeftTrigger2  = 5
    | RightTrigger1 = 6
    | RightTrigger2 = 7
    | LeftStick     = 8
    | RightStick    = 9
    | DPadUp        = 10
    | DPadRight     = 11
    | DPadDown      = 12
    | DPadLeft      = 13
    | Select        = 14
    | BigButton     = 15
    | Start         = 16

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
    Buttons:    list<GamePad * ButtonState * 'Action>
    ThumbStick: GamePadThumbStick<'Action>
    Trigger:    GamePadTriggers<'Action>
}

type FMouseAction<'Action> =
    | Screen of (Vector2 -> 'Action)
    | World  of (Vector2 -> 'Action)

type MouseButton =
    | Left     = 0
    | Middle   = 1
    | Right    = 2
    | XButton1 = 3
    | XButton2 = 4

type InputMouse<'Action> = {
    Buttons:     list<MouseButton * ButtonState * FMouseAction<'Action>>
    ScrollWheel: option<float32 -> 'Action>
    Position:    option<Vector2 -> 'Action>
}

type Input<'Action> = {
    Keyboard: list<Key * ButtonState * 'Action>
    GamePad:  InputGamePad<'Action>
    Mouse:    InputMouse<'Action>
}

module Input =
    let inline cb (b:CBool) : bool = CBool.op_Implicit b

    let gamepadToRaylib gamepad =
        match gamepad with
        | GamePad.ButtonUp      -> GamepadButton.RightFaceUp
        | GamePad.ButtonRight   -> GamepadButton.RightFaceRight
        | GamePad.ButtonDown    -> GamepadButton.RightFaceDown
        | GamePad.ButtonLeft    -> GamepadButton.RightFaceLeft
        | GamePad.LeftTrigger1  -> GamepadButton.LeftTrigger1
        | GamePad.LeftTrigger2  -> GamepadButton.LeftTrigger2
        | GamePad.RightTrigger1 -> GamepadButton.RightTrigger1
        | GamePad.RightTrigger2 -> GamepadButton.RightTrigger2
        | GamePad.LeftStick     -> GamepadButton.LeftThumb
        | GamePad.RightStick    -> GamepadButton.RightThumb
        | GamePad.DPadUp        -> GamepadButton.LeftFaceUp
        | GamePad.DPadRight     -> GamepadButton.LeftFaceRight
        | GamePad.DPadDown      -> GamepadButton.LeftFaceDown
        | GamePad.DPadLeft      -> GamepadButton.LeftFaceLeft
        | GamePad.Select        -> GamepadButton.MiddleLeft
        | GamePad.BigButton     -> GamepadButton.Middle
        | GamePad.Start         -> GamepadButton.MiddleRight
        | _                     -> failwith "Missing GamePad mapping to Raylib"

    let mouseToRaylib mouse : Raylib_cs.MouseButton =
        match mouse with
        | MouseButton.Left     -> Raylib_cs.MouseButton.Left
        | MouseButton.Middle   -> Raylib_cs.MouseButton.Middle
        | MouseButton.Right    -> Raylib_cs.MouseButton.Right
        | MouseButton.XButton1 -> Raylib_cs.MouseButton.Back
        | MouseButton.XButton2 -> Raylib_cs.MouseButton.Side
        | _                    -> failwith "Missing Mouse Button mapping to Raylib"

    /// Transforms MouseAction to desired WorldSpace or ScreenSpace
    let mousePos ma =
        match ma with
        | World  f -> f (Raylib.GetScreenToWorld2D(Raylib.GetMousePosition(), State.camera))
        | Screen f -> f (Raylib.GetMousePosition())

    let getActions (input:Input<'Action>) =
        let actions = ResizeArray<_>()

        // Process Keyboard inputs
        for (key,state,action) in input.Keyboard do
            match state with
                | IsPressed  -> if cb <| Raylib.IsKeyPressed(key)  then actions.Add(action)
                | IsReleased -> if cb <| Raylib.IsKeyReleased(key) then actions.Add(action)
                | IsKeyDown  -> if cb <| Raylib.IsKeyDown(key)     then actions.Add(action)
                | IsKeyUp    -> if cb <| Raylib.IsKeyUp(key)       then actions.Add(action)

        // Process GamePad inputs
        for (button,state,action) in input.GamePad.Buttons do
            match state with
                | IsPressed  -> if cb <| Raylib.IsGamepadButtonPressed( 0, gamepadToRaylib button) then actions.Add(action)
                | IsReleased -> if cb <| Raylib.IsGamepadButtonReleased(0, gamepadToRaylib button) then actions.Add(action)
                | IsKeyDown  -> if cb <| Raylib.IsGamepadButtonDown(    0, gamepadToRaylib button) then actions.Add(action)
                | IsKeyUp    -> if cb <| Raylib.IsGamepadButtonUp(      0, gamepadToRaylib button) then actions.Add(action)

        // Get Thumbsticks values
        match input.GamePad.ThumbStick.Left with
        | Some f ->
            let x   = Raylib.GetGamepadAxisMovement(0, GamepadAxis.LeftX)
            let y   = Raylib.GetGamepadAxisMovement(0, GamepadAxis.LeftY)
            let vec = Vector2(x,y)
            actions.Add(f vec)
        | None -> ()

        match input.GamePad.ThumbStick.Right with
        | Some f ->
            let x   = Raylib.GetGamepadAxisMovement(0, GamepadAxis.RightX)
            let y   = Raylib.GetGamepadAxisMovement(0, GamepadAxis.RightY)
            let vec = Vector2(x,y)
            actions.Add(f vec)
        | None -> ()

        // Get Axis values for Left/Right Trigger 2
        match input.GamePad.Trigger.Left with
        | Some f ->
            let axis = Raylib.GetGamepadAxisMovement(0, GamepadAxis.LeftTrigger)
            if axis <> 0f then actions.Add(f axis)
        | None -> ()

        match input.GamePad.Trigger.Right with
        | Some f ->
            let axis = Raylib.GetGamepadAxisMovement(0, GamepadAxis.RightTrigger)
            if axis <> 0f then actions.Add(f axis)
        | None -> ()

        // Get Mouse input
        for (key,state,action) in input.Mouse.Buttons do
            match state with
            | IsPressed  -> if cb <| Raylib.IsMouseButtonPressed (mouseToRaylib key) then actions.Add(mousePos action)
            | IsReleased -> if cb <| Raylib.IsMouseButtonReleased(mouseToRaylib key) then actions.Add(mousePos action)
            | IsKeyDown  -> if cb <| Raylib.IsMouseButtonDown    (mouseToRaylib key) then actions.Add(mousePos action)
            | IsKeyUp    -> if cb <| Raylib.IsMouseButtonUp      (mouseToRaylib key) then actions.Add(mousePos action)

        input.Mouse.ScrollWheel
        |> Option.iter(fun f -> actions.Add (f (Raylib.GetMouseWheelMove())))

        input.Mouse.Position
        |> Option.iter(fun f -> actions.Add (f (Raylib.GetMousePosition())))

        actions

type FPS = {
    mutable Updates:     int
    mutable ElapsedTime: float
    mutable UpdateFPS:   float
}

module FPS =
    let create (fps:FPS) : FPS = fps

    // Global State
    let state = create {
        Updates     = 0
        ElapsedTime = 0
        UpdateFPS   = 0
    }

    // Called on each update
    let update (deltaTime:float32) =
        let deltaTime = float deltaTime
        state.Updates     <- state.Updates + 1
        state.ElapsedTime <- state.ElapsedTime + deltaTime

        if state.ElapsedTime >= 1.0 then
            state.UpdateFPS   <- float state.Updates / state.ElapsedTime
            state.Updates     <- 0
            state.ElapsedTime <- state.ElapsedTime - 1.0

    let draw () =
        Raylib.DrawText(
            text     = String.Format("FPS: {0:0}", state.UpdateFPS),
            posX     = 3,
            posY     = 3,
            fontSize = 20,
            color    = Color.Yellow
        )

module Gui =
    type Style = {
        FontSize:             int
        FontColor:            Color
        BackgroundColor:      Color
        BackgroundHoverColor: Color
        LineColor:            Color
        LineThickness:        float32
    }

    let mutable style = {
        FontSize             = 12
        FontColor            = Color.Black
        BackgroundColor      = Color.Gray
        BackgroundHoverColor = Color.LightGray
        LineColor            = Color.Black
        LineThickness        = 2f
    }

    let posInRect (pos:Vector2) (rect:Rectangle) : bool =
        if pos.X >= rect.X
           && pos.X <= (rect.X + rect.Width)
           && pos.Y >= rect.Y
           && pos.Y <= (rect.Y + rect.Height) then true else false

    let mouseInRect (rect:Rectangle) =
        posInRect (Raylib.GetMousePosition()) rect

    let button (rect:Rectangle) (text:string) : bool =
        let isHover = mouseInRect rect

        if isHover
        then Raylib.DrawRectangleRec(rect, style.BackgroundHoverColor)
        else Raylib.DrawRectangleRec(rect, style.BackgroundColor)

        let tw = float32 <| Raylib.MeasureText(text, style.FontSize)
        let yText = rect.Y + (rect.Height / 2f - (float32 style.FontSize / 2f))
        let xText = rect.X + (rect.Width - tw) / 2f
        Raylib.DrawText(text, int xText, int yText, style.FontSize, style.FontColor)
        Raylib.DrawRectangleLinesEx(rect, 2f, style.LineColor)

        if CBool.op_Implicit <| (Raylib.IsMouseButtonPressed(Raylib_cs.MouseButton.Left))
        then isHover
        else false