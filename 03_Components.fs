namespace MyGame
open System.Numerics

(* Design Philosophy

1. Functions that mutate a record must return `unit`.

This makes it obvious that a record is being mutated. But best is to avoid
creating mutating functions. I anyway don't expect to write a getter/setter for
every field. Just use the default F# way to access or set a field. Its a
built-in language feature, so use it. Same goes for immutable records with
new fields. Use the record `with` syntax.

Avoid writing mutating functions doesn't mean to avoid mutation. It just means
there is no need for a function that just assigns a value to a mutable field.
Just write `t.Field <- whatever`. There is no need for having a function that
does this.

Only write functions when they add anything useful. This means some kind of
input type conversation/transformation or providing some defaults.
*)

type Texture2D = Raylib_cs.Texture2D
type Rectangle = Raylib_cs.Rectangle
type Color     = Raylib_cs.Color

[<AutoOpen>]
module Helper =
    let private rng = System.Random ()
    /// random integer between min and max both inclusive
    let randi min max = min + (rng.Next() % (max-min+1))
    /// random float between min and max both inclusive
    let randf min max = min + (rng.NextSingle() * (max-min))

module Rad =
    let inline wrap (x:float32) : float32<rad> =
        LanguagePrimitives.Float32WithMeasure<rad> x

    /// A turn describes the angle of a circle between 0 and 1.
    /// So 0.25 or 1/4 is a 1/4 circle or 90 degrees. 0.5 is a half-circle and so on.
    let inline fromTurn (x:float32) : float32<rad> =
        wrap (x * System.MathF.Tau)

    let inline fromDeg (degree:float32<deg>) : float32<rad> =
        wrap(degree * System.MathF.PI / 180.0f<deg>)

    let inline toDeg (radiant:float32<rad>) : float32<deg> =
        (float32 radiant) * 180.0f<deg> / System.MathF.PI

module Comp =
    /// <summary>
    /// Expects a `width`, `height` and an `origin`. Returns a `Vector2` that
    /// represents the Position of the choosen Origin.
    ///
    /// <code lang="fsharp">
    /// Comp.originToVector2 100f 100f Center = Vector2( 50f,50f)
    /// Comp.originToVector2 100f 100f Right  = Vector2(100f,50f)
    /// </code>
    /// </summary>
    let originToVector2 width height origin : Vector2 =
        let x,y =
            match origin with
            | TopLeft        ->         0f,          0f
            | Top            -> width / 2f,          0f
            | TopRight       -> width     ,          0f
            | Left           ->         0f, height / 2f
            | Center         -> width / 2f, height / 2f
            | Right          -> width     , height / 2f
            | BottomLeft     ->         0f, height
            | Bottom         -> width / 2f, height
            | BottomRight    -> width     , height
            | Position (x,y) -> x,y
        Vector2(x,y)

    /// default constructors that basically do nothing. It expects a record
    /// and returns it immediately. The whole purpose of this is because sometimes
    /// type-inference on records can break. By writing `Comp.createTransform { ... }`
    /// its like an additional type declaration. The Compiler/IDE immediately knows
    /// which record you wanna create and which fields are needed. Also reads
    /// nicely in written code.
    let createTransform (st:LocalTransform) : Transform = Local st
    let createView      (st:View)      : View      = st
    let createSprite    (st:Sprite)    : Sprite    = st
    let createSheet     (st:Sheet)     : Sheet     =
        if st.Sprites.Length = 0 then
            failwithf "Cannot create Sheet. Must at least contain one Sprite."
        st
    let createSheets    (st:Sheets)    : Sheets =
        if not (Map.containsKey st.Default st.Sheets) then
            failwithf "Cannot create Sheets. Default Sheet '%s' does not exists." st.Default
        if st.Sheets.Count = 0 then
            failwithf "Cannot create Sheets. Sheets must have at least one item."
        st
    let createAnimation (st:Animation) : Animation = st
    let createMovement  (st:Movement)  : Movement  = st

    let createTransformXY x y = createTransform {
        Position = Vector2(x,y)
        Scale    = Vector2.One
        Rotation = 0f<deg>
    }

    let createSpriteTexture (tex:Texture2D) = createSprite {
        Texture = tex
        SrcRect = Rectangle(0f,0f,float32 tex.Width,float32 tex.Height)
    }

    let createViewfromSprite origin (sprite:Sprite) : View = {
        Sprite    = sprite
        Tint      = Color.White
        Rotation  = 0.0f<deg>
        Origin    = originToVector2 (float32 sprite.SrcRect.Width) (float32 sprite.SrcRect.Height) origin
        Scale     = Vector2.One
    }

    let createViewfromSheet index (sheet:Sheet) : View = {
        Sprite =
            Array.tryItem index sheet.Sprites |> Option.defaultWith (fun _ ->
                eprintfn "Index [%d] out of Range. Max index is [%d] at\n%s"
                    index (sheet.Sprites.Length-1) (stackTrace 1)
                if   sheet.Sprites.Length > 0
                then Array.get sheet.Sprites 0
                else failwith "Sheet has no Sprites"
            )
        Tint      = Color.White
        Rotation  = 0.0f<deg>
        Origin    = Vector2.Zero
        Scale     = Vector2.One
    }

    let createAnimationFromSheets (sheets:Sheets) : Animation =
        let sheet = sheets.Sheets.[sheets.Default] // can throw exception
        createAnimation {
            Sheets        = sheets
            CurrentSheet  = sheet
            ElapsedTime   = 0f
            CurrentSprite = 0
            MaxSprites    = sheet.Sprites.Length
        }

    /// Adds a Parent to a Transform. Or overwrites Parent when it already has one.
    let addTransformParent parent t =
        // Actually the Global fields must be calculated by fetching the Transform
        // of the Entity. But i don't have access to that data here. It's only
        // available when i have State. So i just initialize this fields to
        // Vector2(0,0). This is okay, because at the end of every frame the
        // Global fields should be updated/calculated.
        match t with
        | Local t -> Parent {
                Parent          = parent
                Position        = t.Position
                GlobalPosition  = Vector2.Zero
                Scale           = t.Scale
                GlobalScale     = Vector2.One
                Rotation        = t.Rotation
                GlobalRotation  = 0f<deg>
            }
        | Parent t -> Parent {
                Parent          = parent
                Position        = t.Position
                GlobalPosition  = Vector2.Zero
                Scale           = t.Scale
                GlobalScale     = Vector2.One
                Rotation        = t.Rotation
                GlobalRotation  = 0f<deg>
            }

    /// set rotation on a transform by transforming the Vector2 to a rotation
    let inline setTransformRotationV vector (t:Transform) : unit =
        t.Rotation <- (Rad.toDeg (Vector2.angle vector))

    /// Adds rotation to Transform specified in degree
    let inline addRotationRad (rot:float32<rad>) (t:Transform) : unit =
        t.Rotation <- t.Rotation + (Rad.toDeg rot)

    // TODO: addLocalTransform - that applies the current rotation

    /// Generates a Sprite array from a Texture2D
    let createSpritesfromColumnsRows (columns:int) (rows:int) (texture:Texture2D) : Sprite array =
        let width   = (texture.Width  / columns) |> float32
        let height  = (texture.Height / rows   ) |> float32
        let sprites = [|
            for row=0 to rows-1 do
            for col=0 to columns-1 do
                let posx = (float32 col) * width
                let posy = (float32 row) * height
                yield createSprite {
                    Texture = texture
                    SrcRect = Rectangle(posx, posy, width, height)
                }
        |]
        if sprites.Length = 0 then
            failwith "Sheet with 0 Sprites"
        sprites

    let viewWithOrigin name (view:View) : View =
        let width  = view.Sprite.SrcRect.Width
        let height = view.Sprite.SrcRect.Height
        let origin = originToVector2 width height name
        { view with Origin = origin }

    let getSheetSprite idx (sheet:Sheet) : Sprite voption =
        if idx < sheet.Sprites.Length
        then ValueSome sheet.Sprites.[idx]
        else ValueNone

    let inline getSheetLength (sheet:Sheet) : int =
        sheet.Sprites.Length

    let inline getSheetDuration (sheet:Sheet) =
        sheet.FrameDuration * (float32 sheet.Sprites.Length)

    let addSheetToSheets name (sheet: Sheet) sheets : Sheets = {
        sheets with
            Sheets = (Map.add name sheet sheets.Sheets)
    }

    let inline getSheet name (sheets:Sheets) : Sheet option =
        Map.tryFind name sheets.Sheets

    let inline getSheetExn name (sheets:Sheets) : Sheet =
        Map.find name sheets.Sheets

    /// Creates a View from the currently set sprite sheet
    let createViewFromSheets origin (sheets:Sheets) : View =
        let sprite = sheets.Sheets.[sheets.Default].Sprites.[0]
        createView {
            Sprite   = sprite
            Rotation = 0f<deg>
            Tint     = Color.White
            Scale    = Vector2.One
            Origin   = originToVector2 (float32 sprite.SrcRect.Width) (float32 sprite.SrcRect.Height) origin
        }

    let resetAnimation (animation:Animation) : unit =
        animation.CurrentSprite <- 0
        animation.ElapsedTime   <- 0f

    /// Returns the current Sprite in an animation
    let inline getCurrentSpriteAnimation anim : Sprite voption =
        getSheetSprite anim.CurrentSprite anim.CurrentSheet

    /// Advance the animation to the next Sprite
    let setAnimationNextSprite (anim:Animation) : unit =
        if anim.CurrentSheet.IsLoop then
            anim.CurrentSprite <- (anim.CurrentSprite + 1) % anim.MaxSprites
        else
            if anim.CurrentSprite < anim.MaxSprites-1 then
                anim.CurrentSprite <- anim.CurrentSprite + 1

    /// Switch to another sheet animation
    let switchAnimation name (anim:Animation) : unit =
        match getSheet name anim.Sheets with
        | Some sheet ->
            anim.CurrentSheet  <- sheet
            anim.CurrentSprite <- 0
            anim.ElapsedTime   <- 0f
            anim.MaxSprites    <- sheet.Sprites.Length
        | None ->
            let validAnims = System.String.Join(',', Map.keys anim.Sheets.Sheets)
            failwithf "Cannot switch Animation to \"%s\" valid animation are %s" name validAnims

    let createMovementDirection dir = createMovement {
        Direction = (ValueSome (Relative dir));
        Rotation  = ValueNone
    }

    let createMovementRotation rot = createMovement {
        Direction = ValueNone
        Rotation  = (ValueSome rot)
    }

    let createMovementTo (position:Vector2) speed = createMovement {
        Direction = (ValueSome (Absolute (position,speed)))
        Rotation  =  ValueNone
    }

    let createSpritesFromWidthHeight width height (texture:Texture2D) : Sprite array =
        let columns = texture.Width  / width
        let rows    = texture.Height / height
        let (width, height) = float32 width, float32 height
        let sprites = [|
            for row=0 to rows-1 do
            for col=0 to columns-1 do
                let row, col = float32 row, float32 col
                yield createSprite {
                    Texture = texture
                    SrcRect = Rectangle(col*width, row*height, width, height)
                }
        |]
        if sprites.Length = 0 then
            failwith "Sheet with 0 Sprites"
        sprites
