namespace MyGame
open Raylib_cs
open System.Numerics

type HashSet<'a>       = System.Collections.Generic.HashSet<'a>
type Dictionary<'a,'b> = System.Collections.Generic.Dictionary<'a,'b>
type TimeSpan          = System.TimeSpan
type Matrix            = Matrix4x4

[<AutoOpen>]
module Extensions =
    // Milliseconds
    [<Measure>] type ms
    // Radiant and Degree types
    [<Measure>] type rad
    [<Measure>] type deg

    // Perl like op used in defining hashes/Map
    let (=>) x y = x,y

    // Partial Active Patterns
    let (|IsGreater|_|) target input = if input > target then Some input else None
    let (|IsSmaller|_|) target input = if input < target then Some input else None
    let (|IsEqual|_|)   target input = if input = target then Some input else None

    // Common dispatch for Numbers. Do something if number
    // is greater, smaller or equal zero

    /// Checks if number is greater, smaller or equal to zero and
    /// executes corresponding function.
    let inline cmp fGreater fSmaller fEqual (x:float) =
        if   x > 0.0 then fGreater x
        elif x < 0.0 then fSmaller x
        else fEqual x

    /// Checks if number is greater, smaller or equal to zero and
    /// executes corresponding function.
    let inline cmpF fGreater fSmaller fEqual (x:float32) =
        if   x > 0.0f then fGreater x
        elif x < 0.0f then fSmaller x
        else fEqual x

    /// Checks if number is greater, smaller or equal to zero and
    /// executes corresponding function.
    let inline cmpInt fGreater fSmaller fEqual (x:int) =
        if   x > 0 then fGreater x
        elif x < 0 then fSmaller x
        else fEqual x

    // Returns always the same value as a function
    let is x _ = x

    // StackTrace as a String
    let stackTrace (skip:int) : string =
        let skip = if skip < 0 then 1 else skip+1
        let st   = System.Diagnostics.StackTrace(skip, true)
        "  " + System.String.Join("\n  ",
            st.GetFrames() |> Array.map (fun frame ->
                sprintf "%s:%d %s"
                    (System.IO.Path.GetFileName(frame.GetFileName()))
                    (frame.GetFileLineNumber())
                    (frame.GetMethod().Name)
            )
        )

    let inline sec sec : TimeSpan =
        TimeSpan.FromSeconds sec

    let inline clamp (min:float) max value =
        System.Math.Clamp(value, min, max)

    let inline clampF (min:float32) max value =
        System.Math.Clamp(value, min, max)

    let inline nearly target difference value =
        (abs (value - target)) < difference

    let inline notNearly target difference value =
        not (nearly target difference value)

    module Rectangle =
        /// Returns a Rectangle from two Vectors. Correctly computes topLeft
        /// corner from both vectors and sets the width/height
        let fromVectors (v1:Vector2) (v2:Vector2) =
            Rectangle(
                (min v1.X v2.X),     (min v1.Y v2.Y),
                (abs (v1.X - v2.X)), (abs (v1.Y - v2.Y))
            )

    module TimeSpan =
        let oneSecond = sec 1.0

    type Vector2 with
        static member inline create (x:float32) (y:float32) =
            Vector2(x,y)
        static member Multiply (left:Vector2, right:TimeSpan) =
            Vector2.Multiply(left, float32 right.TotalSeconds)

        static member length  (vec:Vector2) = vec.Length ()

        static member Left     = Vector2(-1f,  0f)
        static member Right    = Vector2( 1f,  0f)
        static member Up       = Vector2( 0f, -1f)
        static member Forward  = Vector2( 0f, -1f) // same as Up
        static member Down     = Vector2( 0f,  1f)
        static member Backward = Vector2( 0f,  1f) // same as Down

        static member addX x (vec:Vector2) = Vector2(vec.X+x, vec.Y  )
        static member addY y (vec:Vector2) = Vector2(vec.X  , vec.Y+y)
        static member flipY  (vec:Vector2) = Vector2(vec.X  ,-vec.Y  )

        /// Calculates the angle relative to Vector2.Up Vector2(1,-1)
        static member angle (vec:Vector2) =
            // (System.MathF.Atan2(vec.Y, vec.X) * 1f<rad>
            Vector2.angle2 Vector2.Up vec

        /// Calculates angle between two given vectors
        static member angle2 (v1:Vector2) (v2:Vector2) =
            let dot = v1.X * v2.X + v1.Y * v2.Y
            let det = v1.X * v2.Y - v1.Y * v2.X
            System.MathF.Atan2(det, dot) * 1f<rad>

        static member private rng = System.Random()
        /// creates a random vector where x,y are in range from -1 to 1
        static member random () =
            Vector2.create
                (Vector2.rng.NextSingle() * 2.0f - 1.0f)
                (Vector2.rng.NextSingle() * 2.0f - 1.0f)

        static member randomDirection (scale:float32) =
            Vector2.Normalize (Vector2.random ()) * scale

        /// returns a normalized vector from an angle (radiant) relative to Vector2.up
        static member fromAngleRad (angle: float32<rad>) =
            Vector2.create (System.MathF.Sin(float32 angle)) (-System.MathF.Cos(float32 angle))

        /// returns a normalized vector from an angle (degree) relative to Vector2.up
        static member fromAngleDeg (angle: float32<deg>) =
            let rad = float32(float angle * System.Math.PI / 180.0) * 1f<rad>
            Vector2.fromAngleRad rad

    type System.Collections.Generic.Dictionary<'a,'b> with
        /// add or overwrites value for specified key. return value of `true`
        /// inidicates that the key was added to the Dictionary
        static member inline add key value (dic:Dictionary<'a,'b>) : unit =
            if dic.ContainsKey(key)
            then dic.[key] <- value
            else dic.Add(key,value)

        static member inline remove key (dic:Dictionary<'a,'b>) =
            dic.Remove(key) |> ignore

        static member change key f (dic:Dictionary<'a,'b>) =
            match dic.TryGetValue key with
            | true, value ->
                match f (ValueSome value) with
                | ValueNone   -> ignore (dic.Remove key)
                | ValueSome x -> dic.[key] <- x
            | false, _ ->
                match f ValueNone with
                | ValueNone   -> ()
                | ValueSome x -> dic.Add(key, x)

        static member inline changeValue defaultValue key ([<InlineIfLambda>] f) (dic:Dictionary<'a,'b>) =
            match dic.TryGetValue key with
            | true, value -> dic.[key] <- f value
            | false, _    -> dic.Add(key, f defaultValue)

        static member inline get key (dic:Dictionary<'a,'b>) =
            match dic.TryGetValue key with
            | true, value -> ValueSome value
            | false, _    -> ValueNone

    module Map =
        /// fetches key from map and applies function `f` to it. When no value for
        /// key was added then it uses the initial value.
        let changeValue (init:'Value) (key:'Key) f map =
            Map.change key (function
                | None   -> Some (f init)
                | Some x -> Some (f x)
            ) map

    module HashSet =
        /// returns a new HashSet that only contains elements that appears in both
        let intersect (x:HashSet<'a>) (y:HashSet<'a>) (c:Camera3D)=
            let smaller, greater =
                if x.Count < y.Count then x,y else y,x

            let newHashSet = HashSet<'a>()
            for x in smaller do
                if greater.Contains x then
                    ignore (newHashSet.Add x)

            newHashSet

        /// shallow copy of a HashSet
        let clone (set:HashSet<'a>) =
            let nh = HashSet()
            for x in set do
                nh.Add x |> ignore
            nh

        /// returns a new HashSet of elements that appear in all given HashSets
        let intersectMany (sets:seq<HashSet<'a>>) =
            if Seq.isEmpty sets then
                HashSet()
            else
                let smallest = clone (sets |> Seq.minBy (fun set -> set.Count))
                for set in sets do
                    smallest.IntersectWith set
                smallest
