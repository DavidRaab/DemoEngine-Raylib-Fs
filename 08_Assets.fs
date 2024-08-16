namespace MyGame
open Raylib_cs

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
    let inline ms time : float32 =
        time * 0.001f

    let load () : Assets =
        let load (str:string) = Raylib.LoadTexture str
        let texture width height color =
            Raylib.GenImageColor(width, height, color)
            |> Raylib.LoadTextureFromImage
        let tint image tint =
            let mutable copy = Raylib.ImageCopy image
            Raylib.ImageColorTint(&copy, tint)
            copy

        let enemy = Raylib.LoadImage("Content/enemy.png")
        let enemy_sprites = [|
            Comp.createSpriteTexture(Raylib.LoadTextureFromImage(tint enemy Color.White))
            Comp.createSpriteTexture(Raylib.LoadTextureFromImage(tint enemy Color.Red  ))
            Comp.createSpriteTexture(Raylib.LoadTextureFromImage(tint enemy Color.Blue ))
        |]
        Raylib.UnloadImage(enemy)

        let assets = {
            Sprites = {
                Pixel    = Comp.createSpriteTexture (texture  1  1 Color.White)
                Missing  = Comp.createSpriteTexture (texture  1  1 Color.Pink)
                WhiteBox = Comp.createSpriteTexture (texture 10 10 Color.White)
                Arrow    = Comp.createSpriteTexture (load "Content/arrow.png")
            }
            Knight = Comp.createSheets {
                Default = "Idle"
                Sheets  = Map [
                    "Idle"   => Comp.createSheet {
                        FrameDuration = (ms 100f); IsLoop =  true;
                        Sprites = (Comp.createSpritesfromColumnsRows 10 1 (load "Content/FreeKnight/Idle.png"))
                    }
                    "Attack" => Comp.createSheet {
                        FrameDuration =  (ms 50f); IsLoop = false;
                        Sprites = (Comp.createSpritesfromColumnsRows  4 1 (load "Content/FreeKnight/Attack.png"))
                    }
                    "Run"    => Comp.createSheet {
                        FrameDuration = (ms 100f); IsLoop =  true;
                        Sprites = (Comp.createSpritesfromColumnsRows 10 1 (load "Content/FreeKnight/Run.png"))
                    }
                    "Crouch" => Comp.createSheet {
                        FrameDuration =   (ms 0f); IsLoop = false;
                        Sprites = [| Comp.createSpriteTexture             (load "Content/FreeKnight/Crouch.png") |]
                    }
                ]
            }
            Box = Comp.createSheets {
                Default = "Default"
                Sheets  = Map [
                    "Default" =>
                        Comp.createSheet { FrameDuration = (ms 250f); IsLoop = true; Sprites = enemy_sprites
                    }
                ]
            }
        }
        assets