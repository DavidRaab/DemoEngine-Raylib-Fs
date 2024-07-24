namespace MyGame
open Microsoft.Xna.Framework
open Microsoft.Xna.Framework.Graphics
open Microsoft.Xna.Framework.Input

type RenderTarget = {
    Width:  int
    Height: int
}

type MonoGame<'Assets,'GameState>(init, loadAssets, initModel, update, draw, renderTarget:RenderTarget) as this =
    inherit Game()
    let graphics             = new GraphicsDeviceManager(this)
    let mutable sb           = Unchecked.defaultof<SpriteBatch>
    let mutable assets       = Unchecked.defaultof<'Assets>
    let mutable model        = Unchecked.defaultof<'GameState>
    let internalAspectRatio  = float renderTarget.Width / float renderTarget.Height

    member this.Graphics     = graphics
    member this.Asset        = assets
    member this.SpriteBatch  = sb
    member this.Gd           = this.GraphicsDevice
    member this.RenderTarget = renderTarget

    member this.CalculateViewport () =
        let windowWidth, windowHeight =
            this.Graphics.PreferredBackBufferWidth,
            this.Graphics.PreferredBackBufferHeight
        let clientAspectRatio = float windowWidth / float windowHeight

        // The internal renderTarget needs to be scaled. But either the width
        // or the height will be the same as the window size. The other value
        // needs to be calculated.
        let width, height =
            if internalAspectRatio <= clientAspectRatio then
                let h = windowHeight
                let w = int ((float h) * internalAspectRatio + 0.5)
                w,h
            else
                let w = windowWidth
                let h = int ((float w) / internalAspectRatio + 0.5)
                w,h

        // Set the viewport and always center the viewport in the current window
        this.GraphicsDevice.Viewport <- Viewport(
            x        = (windowWidth  / 2) - (width  / 2),
            y        = (windowHeight / 2) - (height / 2),
            width    = width,
            height   = height,
            MinDepth = 0.0f,
            MaxDepth = 1.0f
        )

    // initialize the game and load any needed non-graphical resources.
    override this.Initialize () =
        init this
        base.Initialize ()

    // called after initialize but before first Update
    override this.BeginRun () =
        this.CalculateViewport ()

    // load graphical resources required by the game
    override this.LoadContent () =
        sb     <- new SpriteBatch(this.GraphicsDevice)
        assets <- loadAssets this
        model  <- initModel assets
        base.LoadContent ()

    // Called when the game should update.
    override this.Update(gameTime) =
        model <- update model gameTime this
        base.Update gameTime

    // Called when the game should draw a frame.
    override this.Draw gameTime =
        draw model gameTime this
        base.Draw gameTime

    member this.SetResolution x y =
        graphics.PreferredBackBufferWidth  <- x
        graphics.PreferredBackBufferHeight <- y
        graphics.ApplyChanges ()

    member this.GetResolution () =
        graphics.PreferredBackBufferWidth, graphics.PreferredBackBufferHeight
