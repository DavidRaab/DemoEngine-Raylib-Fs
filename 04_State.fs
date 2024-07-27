namespace MyGame.State
open Raylib_cs
open Dic2
open MyGame
open MyGame.DataTypes

type Dictionary<'a,'b> = System.Collections.Generic.Dictionary<'a,'b>

module State =
    let mutable camera   = Unchecked.defaultof<Camera2D>
    let mutable uiCamera = Unchecked.defaultof<Camera2D>
    let Transform        = Dictionary<Entity,Transform>()
    let Movement         = Dictionary<Entity,Movement>()
    let Animation        = Dictionary<Entity,Animation>()
    // (bool * Layer) represents IsVisible * Layer
    let View             = Dic2.create<(bool * Layer), Entity, View>()

    // let View          = State<View>()
    (*
    module View =
        let visible = Dictionary<Entity,View>()
        let hidden  = Dictionary<Entity,View>()

        /// Removes the View for an Entity
        let inline remove (e:Entity) : unit =
            visible.Remove(e) |> ignore
            hidden.Remove(e)  |> ignore

        /// Adds a View for an Entity
        let add (e:Entity) (isVisible:bool) (v:View) : unit =
            let ev = visible.ContainsKey(e)
            let eh = hidden.ContainsKey(e)

            if isVisible then
                if ev then visible.[e] <- v
                      else visible.Add(e,v)
                if eh then hidden.Remove(e) |> ignore
            else
                if ev then visible.Remove(e) |> ignore
                if eh then hidden.[e] <- v
                      else hidden.Add(e,v)

        /// Get visibility and View of an Entity. If no View is present returns ValueNone
        let get (e:Entity) : (bool * View) voption =
            match visible.TryGetValue(e) with
            | true, view -> ValueSome (true,view)
            | false, _   ->
                match hidden.TryGetValue(e) with
                | true, view -> ValueSome (false,view)
                | false, _   -> ValueNone

        /// Returns if Entity is Visible. Also returns false when no View was added for an Entity.
        let inline isVisible (e:Entity) =
            visible.ContainsKey(e)

        /// Sets Visibility. Does nothing when no View was added for an Entity.
        let inline setVisiblity (e:Entity) (should_be_visible:bool) =
            match get e with
            | ValueSome(_,view) -> add e should_be_visible view
            | ValueNone         -> ()

        /// Switches visibility. Does nothing when no View was added for an Entity.
        let inline switchVisibility (e:Entity) =
            match get e with
            | ValueSome(true,view) ->
                visible.Remove(e) |> ignore
                hidden.Add(e,view)
            | ValueSome(false,view) ->
                hidden.Remove(e) |> ignore
                visible.Add(e,view)
            | ValueNone -> ()

        /// Get View, runs function on it, and stores the new View returned by it
        let inline map ([<InlineIfLambda>] f: View -> View) (e:Entity) =
            match get e with
            | ValueSome(true, view) -> visible.[e] <- (f view)
            | ValueSome(false,view) -> hidden.[e]  <- (f view)
            | ValueNone             -> ()

        /// Get view and runs a function on it. Does nothing when no View was added for an Entity
        let inline iter ([<InlineIfLambda>] f: View -> unit) (e:Entity) =
            match get e with
            | ValueSome(_,view) -> f view
            | ValueNone         -> ()
        *)
