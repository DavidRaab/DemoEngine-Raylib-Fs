namespace MyGame.Assets
open Raylib_cs
open MyGame.Extensions
open MyGame.DataTypes
open MyGame.Components

type Assets = {
    Sprites: Sprites
    Knight:  Sheets
    Box:     Sheets
}
and Sprites = {
    Pixel:    Sprite
    Missing:  Sprite
    Arrow:    Sprite
    WhiteBox: Sprite
}

module Assets =
    let inline ms time : TimeSpan =
        TimeSpan.FromMilliseconds(time)

    let load () : Assets =
        let load (str:string) = Raylib.LoadTexture str
        let texture width height color =
            Raylib.GenImageColor(width, height, color)
            |> Raylib.LoadTextureFromImage

        let assets = {
            Sprites = {
                Pixel    = Sprite.fromTexture (texture  1  1 Color.White)
                Missing  = Sprite.fromTexture (texture  1  1 Color.Pink)
                WhiteBox = Sprite.fromTexture (texture 10 10 Color.White)
                Arrow    = Sprite.fromTexture (load "Content/arrow.png")
            }
            Knight = Sheets.create {
                Default = "Idle"
                Sheets  = Map [
                    "Idle"   => Sheet.create { FrameDuration = (ms 100); IsLoop =  true; Sprites = (Sprite.fromColumnsRows 10 1 (load "Content/FreeKnight/Idle.png"))   }
                    "Attack" => Sheet.create { FrameDuration =  (ms 50); IsLoop = false; Sprites = (Sprite.fromColumnsRows  4 1 (load "Content/FreeKnight/Attack.png")) }
                    "Run"    => Sheet.create { FrameDuration = (ms 100); IsLoop =  true; Sprites = (Sprite.fromColumnsRows 10 1 (load "Content/FreeKnight/Run.png"))    }
                    "Crouch" => Sheet.create { FrameDuration =   (ms 0); IsLoop = false; Sprites = [| Sprite.fromTexture        (load "Content/FreeKnight/Crouch.png") |] }
                ]
            }
            Box = Sheets.create {
                Default = "Default"
                Sheets  = Map [
                    "Default" =>
                        Sheet.create { FrameDuration = (ms 250); IsLoop = true; Sprites = [|
                            Sprite.fromTexture (texture 10 10 Color.White)
                            Sprite.fromTexture (texture 10 10 Color.Red)
                            Sprite.fromTexture (texture 10 10 Color.Blue)
                        |]
                    }
                ]
            }
        }
        assets