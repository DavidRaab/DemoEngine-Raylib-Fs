open Raylib_cs
open Animation

// importing only types i need
type String      = System.String
type Vector2     = System.Numerics.Vector2
type HashSet<'a> = System.Collections.Generic.HashSet<'a>

Raylib.InitWindow(800, 480, "Hello World");

let inline sec time =
    TimeSpan.FromSeconds(time)

let position =
    Animation.zip
        (Animation.loop (Animation.roundTrip 100 750 (sec 3.0)))
        (Animation.loop (Animation.roundTrip 50  350 (sec 1.0)))

let radius =
    Animation.roundTrip 1.0 100.0 (sec 3.0)
    |> Animation.loop
    |> Animation.map (fun x -> float32 x)

let animation =
    Animation.zip position radius

let circle = Animation.run animation

let cb (b:CBool) : bool =
    CBool.op_Implicit(b)


type DrawLine =
    | NoLine
    | MouseStart    of Vector2
    | MouseMovement of Vector2 * Vector2
    | MouseStop     of Vector2 * Vector2

let lines = ResizeArray<_>()
let mutable line = NoLine
let mutable running = 0f
let mutable fps = 0
let mutable current_fps = 0
while not (Raylib.WindowShouldClose () |> cb) do
    Raylib.BeginDrawing ()
    Raylib.ClearBackground (Color.White)
    
    let deltaTime = Raylib.GetFrameTime ()

    // Get position when mouse is pressed
    let mouse = Raylib.GetMousePosition ()
    if cb <| Raylib.IsMouseButtonDown(MouseButton.Left) then
        line <-
            match line with
            | NoLine                  -> MouseStart    (mouse)
            | MouseStart start        -> MouseMovement (start,mouse)
            | MouseMovement (start,_) -> MouseMovement (start,mouse)
            | MouseStop         (_,_) -> MouseStart    (mouse)

    if cb <| Raylib.IsMouseButtonReleased(MouseButton.Left) then
        line <-
            match line with
            | NoLine                  -> NoLine
            | MouseStart      (start) -> MouseStop (start,mouse)
            | MouseMovement (start,_) -> MouseStop (start,mouse)
            | MouseStop (start,stop)  -> MouseStop (start,stop)

        match line with
        | MouseStop (start,stop) -> lines.Add (start, stop)
        | _                      -> ()

    // Draw current line
    match line with
    | NoLine           -> ()
    | MouseStart     _ -> ()
    | MouseMovement (start,stop)
    | MouseStop     (start,stop)  ->
        Raylib.DrawLine(int start.X, int start.Y, int stop.X, int stop.Y, Color.Black)

    // Draw stored lines
    for (start,stop) in lines do
        Raylib.DrawLine(int start.X, int start.Y, int stop.X, int stop.Y, Color.Black)

    // Reset lines on right mouse button click
    if cb <| Raylib.IsMouseButtonDown(MouseButton.Right) then
        line <- NoLine
        lines.Clear()

    // Calculate FPS
    running <- running + deltaTime
    if running >= 1f then
        running     <- running - 1f
        current_fps <- fps
        fps         <- 0
    fps <- fps + 1

    Raylib.DrawText(
        ("FPS: " + (string current_fps)),
        0,0, 20, Color.Black)

    match Anim.runTime (float deltaTime) circle with
    | Running ((x,y),radius) -> Raylib.DrawCircle(int x, int y, radius, Color.Black)
    | Finished _             -> ()

    Raylib.EndDrawing ()

Raylib.CloseWindow()
