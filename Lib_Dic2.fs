namespace Dic2

type Dic<'a,'b> = System.Collections.Generic.Dictionary<'a,'b>

module Dic2 =
    type T<'KeyA,'KeyB,'Value> = {
        BToA: Dic<'KeyB,'KeyA>
        Data: Dic<'KeyA,Dic<'KeyB,'Value>>
    }

    let create<'KeyA, 'KeyB, 'Value when 'KeyA:equality and 'KeyB:equality>() : T<'KeyA,'KeyB,'Value> = {
        BToA = Dic<'KeyB, 'KeyA>()
        Data = Dic<'KeyA, Dic<'KeyB, 'Value>>()
    }

    // I fucking hate who ever dumbass thought it is a good idea to throw an
    // exception when you add a key that already exists. Just overwrite that
    // fucking thing.

    /// Adds a key to a dictionary
    let inline dicAdd key value (dic:Dic<'Key,'Value>) =
        if dic.ContainsKey(key)
        then dic.[key] <- value
        else dic.Add(key,value)

    /// Trys to fetch a key from a Dictionary, when it does not exists it executes
    /// an initialization function and returns this value
    let inline dicGetOrInit key (dic:Dic<'Key,'Value>) ([<InlineIfLambda>] init: unit -> 'Value) : 'Value =
        match dic.TryGetValue(key) with
        | true, value -> value
        | false, _    ->
            let value = init ()
            dic.Add(key,value)
            value

    let clear data : unit =
        data.BToA.Clear()
        data.Data.Clear()

    let count data : int =
        // Also could just return BToA.Count
        let mutable count = 0
        for KeyValue(_,inner) in data.Data do
            count <- count + inner.Count
        count

    let contains (key:'KeyB) (data:T<'KeyA,'KeyB,'Value>) =
        match data.BToA.TryGetValue(key) with
        | false, _   -> false
        | true, keyA ->
            match data.Data.[keyA].TryGetValue(key) with
            | false, _ -> false
            | true,  _ -> true

    let get (key:'KeyB) data : voption<'KeyA * 'Value> =
        match data.BToA.TryGetValue(key) with
        | false, _   -> ValueNone
        | true, keyA ->
            match data.Data.[keyA].TryGetValue(key) with
            | false, _     -> ValueNone
            | true,  value -> ValueSome (keyA,value)

    let remove (key:'KeyB) data : unit =
        match data.BToA.TryGetValue(key) with
        | false, _   -> ()
        | true, keyA ->
            data.Data.[keyA].Remove(key) |> ignore
            data.BToA.Remove(key)        |> ignore

    let add (keyA:'KeyA) (keyB:'KeyB) (value:'Value) data : unit =
        match data.BToA.TryGetValue(keyB) with
        | false, _ ->
            let inner = dicGetOrInit keyA data.Data (fun () -> Dic())
            dicAdd keyB keyA  data.BToA
            dicAdd keyB value inner
        | true, mapping ->
            if mapping = keyA then
                dicAdd keyB value data.Data.[keyA]
            else
                data.Data.[mapping].Remove(keyB) |> ignore
                let inner = dicGetOrInit keyA data.Data (fun () -> Dic())
                dicAdd keyB keyA  data.BToA
                dicAdd keyB value inner

    let inline iter (keyA:'KeyA) ([<InlineIfLambda>] f : 'KeyB -> 'Value -> unit) data : unit =
        match data.Data.TryGetValue(keyA) with
        | false, _    -> ()
        | true, inner ->
            for KeyValue(keyB,value) in inner do
                f keyB value