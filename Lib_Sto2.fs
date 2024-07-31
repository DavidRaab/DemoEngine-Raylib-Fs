namespace Sto2
open Storage

(* This Data-Structure basically just emulates a hash of a hash. When
   you ever worked with Perl, like me, then it is painfully to see
   how annoying it is to create such structures in other languages.

   Basically all this module do would be achievable in Perl by:

   $hash->{$layer}{$entity} = $whatever;

   Its a hash that has a key by layer and it returns another hash containing
   $entity -> $whatever mappings. The difference is, when you access a non-existing
   key, let's say $hash->{$layer} was never initiliazed before. But you use it
   as a hash. Then Perl initialize a Hash for you. So the above will work
   and initialize a Hash of Hash. It works with any kind of depth, including
   arrays. It seems to me that Perl is the only language that gets creating
   complex data-structures right.

   In nearly all other languages this throws errors. Also in JavaScript
   for example you need to create all intermediate entries first.

   var data = {};
   data[layer] = {};
   data[layer][entity] = whatever;

   or basically you always need to check with exhaustive if/else branches
   if a structure was created and so on. So annyoing to work with. Same
   goes in F#/C#. You have to create all intermediate structures or it will
   throw exceptions at you.

   This modules tries to eliminate this pain, and gives a two dimensional
   storage.

   You also could create a key that combines layer and entity as a Tuple. But
   this kind of storage gives you no easy way to just ask for all entries of
   a specific layer. Like give me all entity->whatever mapping for LayerA.
   But this is exactly what I need and this module can do.
*)

type Dic<'a,'b> = System.Collections.Generic.Dictionary<'a,'b>
type Sto<'a,'b> = Storage.T<'a,'b>

module Sto2 =
    type T<'KeyA,'KeyB,'Value> = {
        BToA: Dic<'KeyB,'KeyA>
        Data: Dic<'KeyA,Sto<'KeyB,'Value>>
    }

    let create<'KeyA, 'KeyB, 'Value when 'KeyA:equality and 'KeyB:equality>() : T<'KeyA,'KeyB,'Value> = {
        BToA = Dic<'KeyB, 'KeyA>()
        Data = Dic<'KeyA, Sto<'KeyB, 'Value>>()
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
            count <- count + Storage.length inner
        count

    /// Returns a boolean indicating if keyB is present in collection
    let contains (key:'KeyB) (data:T<'KeyA,'KeyB,'Value>) =
        match data.BToA.TryGetValue(key) with
        | false, _   -> false
        | true, keyA -> Storage.contains key data.Data.[keyA]

    /// Get an Entry by specifiyng KeyB, it returns KeyA and the Value
    let get (key:'KeyB) data : voption<'KeyA * 'Value> =
        match data.BToA.TryGetValue(key) with
        | false, _   -> ValueNone
        | true, keyA ->
            match Storage.get key data.Data.[keyA] with
            | ValueSome value -> ValueSome (keyA,value)
            | ValueNone       -> ValueNone

    let remove (key:'KeyB) data : unit =
        match data.BToA.TryGetValue(key) with
        | false, _   -> ()
        | true, keyA ->
            Storage.remove key data.Data.[keyA]
            data.BToA.Remove(key) |> ignore

    let add (keyA:'KeyA) (keyB:'KeyB) (value:'Value) data : unit =
        match data.BToA.TryGetValue(keyB) with
        | false, _ ->
            let inner = dicGetOrInit keyA data.Data (fun () -> Storage.create ())
            dicAdd keyB keyA data.BToA
            Storage.insert keyB value inner
        | true, mapping ->
            if mapping = keyA then
                Storage.insert keyB value data.Data.[keyA]
            else
                Storage.remove keyB data.Data.[mapping]
                let inner = dicGetOrInit keyA data.Data (fun () -> Storage.create ())
                dicAdd keyB keyA  data.BToA
                Storage.insert keyB value inner

    let inline iter (keyA:'KeyA) ([<InlineIfLambda>] f : 'KeyB -> 'Value -> unit) data : unit =
        match data.Data.TryGetValue(keyA) with
        | false, _    -> ()
        | true, inner -> Storage.iter f inner