namespace Animation

type TimeSpan = System.TimeSpan

[<StructuralEquality; StructuralComparison>]
[<Struct>]
type AnimationResult<'a> =
    | Running  of value:'a
    | Finished of struct ('a * TimeSpan)

[<NoEquality;NoComparison>]
type Anim<'a>      = Anim      of (TimeSpan -> AnimationResult<'a>)
[<NoEquality;NoComparison>]
type Animation<'a> = Animation of (unit     -> Anim<'a>)

module Anim =
    let run deltaTime (Anim f) =
        f deltaTime

    let runTime seconds anim =
        run (TimeSpan.FromSeconds seconds) anim

    let value anim =
        match anim with
        | Running  v     -> v
        | Finished (v,_) -> v

    let inline running x    = Running x
    let inline finished x t = Finished (x,t)

module Ease =
    let PI = System.Math.PI

    let none = id

    let inSine x =
        1.0 - cos ((x * PI) / 2.0)

module Animation =
    /// Turns a single value into an animation
    let wrap x =
        Animation(fun () -> Anim(fun deltaTime -> Finished(x,deltaTime)))

    /// Repeats `x` for `duration` time
    let duration duration x =
        Animation(fun () ->
            let mutable soFar = TimeSpan.Zero
            Anim(fun dt ->
                soFar <- soFar + dt
                if   soFar < duration
                then Anim.running  x
                else Anim.finished x (soFar - duration)
            )
        )

    /// Runs an animation by returning Anim<'a>
    let run (Animation f) =
        f ()

    /// You directly pass the `time` value to an `animation` and get the value
    /// at the specified animation time. Probably only useful in testing. For normal
    /// animation you should call `Animation.run` on the `animation` and use `Anim.run`
    /// instead.
    let execute time animation =
        Anim.run time (run animation)

    /// turns a function accepting a TimeSpan into an animation
    let create f =
        Animation(fun () -> Anim f)

    /// Transforms a lerping function `f` into an animation. It takes `duration` time
    /// to run from 0.0 to 1.0. Before this value is passed to the lerping function an `easing`
    /// function is applied to the value.
    let lerpWith (easing:float->float) (duration:TimeSpan) f =
        Animation(fun () ->
            let mutable soFar = TimeSpan.Zero
            Anim(fun dt ->
                soFar <- soFar + dt
                if   soFar < duration
                then Anim.running  (f (easing (soFar / duration)))
                else Anim.finished (f (easing 1.0)) (soFar - duration)
            )
        )

    /// Transforms a lerping function into an animation. It takes `duration` time
    /// to run from 0.0 to 1.0. This is passed to the lerping function that must
    /// return the actual value.
    let lerp duration f =
        lerpWith Ease.none duration f

    /// runs `animation` with `stepTime` and passes every value to a
    /// `folder` function to compute a final state.
    let fold folder stepTime (state:'State) animation =
        let anim = run animation
        let rec loop state =
            match Anim.run stepTime anim with
            | Running  _ as a -> loop (folder state a)
            | Finished _ as a -> folder state a
        loop state

    /// runs `animation` with `stepTime` and passes every value to a
    /// `folder` function to compute the final state. The `folder` function
    /// starts with the final value running from stop to start.
    let foldBack folder stepTime animation (state:'State) =
        let anim = run animation

        // run animation to build stack
        let stack = System.Collections.Generic.Stack()
        let rec collect () =
            match Anim.run stepTime anim with
            | Running  _ as a -> stack.Push a; collect ()
            | Finished _ as a -> stack.Push a
        collect ()

        // create state from stack
        let rec loop state =
            match stack.TryPop() with
            | true,  x -> loop (folder x state)
            | false, _ -> state
        loop state

    /// Turns a whole animation into an array by simulating it with `stepTime`
    let toArray stepTime anim =
        let ra = ResizeArray<_>()
        fold (fun _ x -> ra.Add(Anim.value x)) stepTime () anim
        ra.ToArray()

    /// Turns a whole animation into a list by simulating it with `stepTime`
    let toList stepTime anim =
        foldBack (fun x xs -> Anim.value x :: xs) stepTime anim []

    /// returns a new animation whose values are applied to the given function.
    let map f anim =
        Animation(fun () ->
            let anim = run anim
            Anim(fun dt ->
                match Anim.run dt anim with
                | Running   x    -> Anim.running  (f x)
                | Finished (x,t) -> Anim.finished (f x) t
            )
        )

    let ap (fanim:Animation<'a -> 'b>) (anim:Animation<'a>) : Animation<'b> =
        Animation(fun () ->
            let fanim = run fanim
            let anim  = run anim
            Anim(fun dt ->
                match Anim.run dt fanim, Anim.run dt anim with
                | Running   f,    Running   x    -> Anim.running  (f x)
                | Running   f,    Finished (x,_) -> Anim.running  (f x)
                | Finished (f,_), Running   x    -> Anim.running  (f x)
                | Finished (f,t), Finished (x,u) -> Anim.finished (f x) (min t u)
            )
        )

    let map2 f anim1 anim2 =
        ap (map f anim1) anim2

    let map3 f a1 a2 a3 =
        ap (ap (map f a1) a2) a3

    let map4 f a1 a2 a3 a4 =
        ap (ap (ap (map f a1) a2) a3) a4

    let map5 f a1 a2 a3 a4 a5 =
        ap (ap (ap (ap (map f a1) a2) a3) a4) a5

    let map6 f a1 a2 a3 a4 a5 a6 =
        ap (ap (ap (ap (ap (map f a1) a2) a3) a4) a5) a6

    let map7 f a1 a2 a3 a4 a5 a6 a7 =
        ap (ap (ap (ap (ap (ap (map f a1) a2) a3) a4) a5) a6) a7

    let map8 f a1 a2 a3 a4 a5 a6 a7 a8 =
        ap (ap (ap (ap (ap (ap (ap (map f a1) a2) a3) a4) a5) a6) a7) a8

    let map9 f a1 a2 a3 a4 a5 a6 a7 a8 a9 =
        ap (ap (ap (ap (ap (ap (ap (ap (map f a1) a2) a3) a4) a5) a6) a7) a8) a9

    /// Like `sequence` but additionally a mapping function is applied to the resulting list
    let traverse f anims =
        let folder a anims =
            map2 (fun x xs -> f x :: xs) a anims
        Seq.foldBack folder anims (wrap [])

    /// Converts a sequence of animations into an animation containing a list
    let sequence anims =
        traverse id anims

    /// Combine two animations by running the first then the second animation
    let append anim1 anim2 =
        Animation(fun () ->
            let anim1 = run anim1
            let anim2 = run anim2

            let mutable first = true
            Anim(fun dt ->
                if first then
                    match Anim.run dt anim1 with
                    | Running x      -> Running x
                    | Finished (x,t) ->
                        first    <- false
                        if t = TimeSpan.Zero
                        then Running x
                        else Anim.run t anim2
                else
                    Anim.run dt anim2
            )
        )

    // concats mutliple animations into a single animation
    let concat anims =
        let anims = Array.ofSeq anims
        if Array.length anims = 0 then
            failwith "Sequence cannot be empty."
        Array.reduce append anims

    /// Applies `duration` to every element and turns it into a single animation
    let concatDuration time xs =
        concat (Seq.map (duration time) xs)

    /// Repeats an animation a given time
    let repeat count anim =
        concat (Array.replicate count anim)

    /// zip two animations
    let zip anim1 anim2 =
        map2 (fun x y -> x,y) anim1 anim2

    /// zip three animations
    let zip3 anim1 anim2 anim3 =
        map3 (fun x y z -> x,y,z) anim1 anim2 anim3

    /// zip four animations
    let zip4 anim1 anim2 anim3 anim4 =
        map4 (fun x y z w -> x,y,z,w) anim1 anim2 anim3 anim4

    /// Animation that runs from start to stop with the given `perSecond`
    let speed start stop perSecond =
        if start <= stop then
            Animation(fun () ->
                let mutable current = start
                Anim(fun dt ->
                    current <- current + (perSecond * dt.TotalSeconds)
                    if   current < stop
                    then Anim.running  current
                    else Anim.finished stop TimeSpan.Zero
                )
            )
        else
            Animation(fun () ->
                let mutable current = start
                Anim(fun dt ->
                    current <- current - (perSecond * dt.TotalSeconds)
                    if   current > stop
                    then Anim.running  current
                    else Anim.finished stop TimeSpan.Zero
                )
            )

    /// Animation from `start` to `stop` in the given `duration` with easing function `ease`
    let rangeWith ease start stop duration =
        lerpWith ease duration (fun fraction ->
            (start * (1.0 - fraction)) + (stop * fraction)
        )

    /// Animation from `start` to `stop` in the given `duration`
    let range start stop duration =
        rangeWith Ease.none start stop duration

    /// Animation from `start` to `stop` in the given `duration`
    let rangeF (start:float32) (stop:float32) duration =
        lerp duration (fun fraction ->
            let fraction = float32 fraction
            float32 ((start * (1.0f - fraction)) + (stop * fraction))
        )

    /// Animation from `start` to `stop` in the given `duration`
    let rangeI (start:int) (stop:int) duration =
        lerp duration (fun fraction ->
            round ((float start * (1.0 - fraction)) + (float stop * fraction))
        )

    /// creates an animation that runs from `start` to `stop` and back to `start` in
    /// the given `duration`.
    let roundTrip start stop (duration:TimeSpan) =
        append
            (range start stop  (duration / 2.0))
            (range stop  start (duration / 2.0))

    /// Loops an animation forever. When the animation finished it starts again
    let loop animation =
        Animation(fun () ->
            let mutable anim = run animation
            Anim(fun dt ->
                match Anim.run dt anim with
                | Running x      -> Anim.running x
                | Finished (x,t) ->
                    if t = TimeSpan.Zero then
                        anim <- run animation
                        Anim.running x
                    else
                        let rec loop t =
                            anim <- run animation
                            match Anim.run t anim with
                            | Running x      -> Anim.running x
                            | Finished (_,t) -> loop t
                        loop t
            )
        )

    /// Takes up to `duration` time of `animation` and then finish. If `animation`
    /// is shorter than `duration` then it also finish earlier.
    let take duration animation =
        Animation(fun () ->
            let anim             = run animation
            let mutable soFar    = TimeSpan.Zero
            let mutable finished = ValueNone
            Anim(fun dt ->
                match finished with
                | ValueSome x -> Anim.finished x dt
                | ValueNone   ->
                    if (soFar + dt) < duration then
                        soFar <- soFar + dt
                        match Anim.run dt anim with
                        | Running x      -> Anim.running x
                        | Finished (x,t) ->
                            finished <- ValueSome x
                            Anim.finished x t
                    else
                        let uptoDuration = duration - soFar
                        let remaining    = dt - uptoDuration
                        match Anim.run uptoDuration anim with
                        | Running x      ->
                            finished <- ValueSome x
                            Anim.finished x remaining
                        | Finished (x,t) ->
                            finished <- ValueSome x
                            Anim.finished x (t+remaining)
            )
        )

    /// Skips `duration` time of `animation`
    let skip duration animation =
        Animation(fun () ->
            let anim = run animation
            ignore (Anim.run duration anim)
            Anim(fun dt ->
                Anim.run dt anim
            )
        )

    /// runs two animations with `stepTime` and check if they produce the same values
    let equal stepTime anim1 anim2 =
        let anim1 = run anim1
        let anim2 = run anim2
        let rec loop () =
            match Anim.run stepTime anim1, Anim.run stepTime anim2 with
            | Running  _     , Finished _      -> false
            | Finished _     , Running  _      -> false
            | Running  (x)   , Running  (y)    -> if x=y          then loop () else false
            | Finished (x,t1), Finished (y,t2) -> if x=y && t1=t2 then true    else false
        loop ()

    /// Transforms the value of the animation to float32
    let inline float32 animation =
        map float32 animation

    /// Transforms the value of the animation to int
    let inline int animation =
        map round animation