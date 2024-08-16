namespace MyGame
open System.Numerics

type Dic<'a,'b> = System.Collections.Generic.Dictionary<'a,'b>

module Dic =
    let getOrInit init key (dic:Dic<'a,'b>) =
        match dic.TryGetValue(key) with
        | true , value -> value
        | false, _     ->
            let value = init ()
            dic.Add(key, value)
            value

    /// Gets key from a Dictionary and assumes it is an array. Creates
    /// array if key was not populated before.
    let getArray key dic =
        getOrInit (fun () -> ResizeArray() ) key dic

    let add key value (dic:Dic<_,_>) =
        match dic.ContainsKey key with
        | true  -> dic.[key] <- value
        | false -> dic.Add(key,value)

    let remove key (dic:Dic<_,_>) =
        dic.Remove(key) |> ignore

    /// Assumes that `key` contains a ResizeArray and pushes a value onto it.
    /// Creates an empty ResizeArray when the key was not populated before.
    let push (key:'Key) (value:'Value) (dic:Dic<_,_>) : unit =
        let ra = getArray key dic
        ra.Add(value)

    let get (key:'Key) (dic:Dic<_,_>) =
        match dic.TryGetValue(key) with
        | false,_ -> ValueNone
        | true, x -> ValueSome x


type STree<'a> = {
    ChunkSize: int
    Chunks:    Dic<struct (int * int), ResizeArray<'a>>
}

module STree =
    let create size = {
        ChunkSize = size
        Chunks    = Dic<_,_>()
    }

    let length tree =
        let mutable count = 0
        for KeyValue(_,ra) in tree.Chunks do
            count <- count + ra.Count
        count

    /// Returns chunk as x,y,w,h. Can be used as input for Rectangle
    let getChunkRegions tree = seq {
        let s = tree.ChunkSize
        for (x,y) in tree.Chunks.Keys do
            (if x > 0 then x-1 else x) * s,
            (if y > 0 then y-1 else y) * s,
            s, s
    }

    /// Calculates the position in the Stree for the Vector2
    let calcPos (vec:Vector2) stree =
        let x =
            if vec.X >= 0f
            then (int vec.X / stree.ChunkSize) + 1
            else (int vec.X / stree.ChunkSize) - 1
        let y =
            if vec.Y >= 0f
            then (int vec.Y / stree.ChunkSize) + 1
            else (int vec.Y / stree.ChunkSize) - 1
        struct (x,y)

    /// Adds an object with position to a spatial tree
    let add (position:Vector2) x stree =
        Dic.push (calcPos position stree) x stree.Chunks

    /// Generates a Spatial Tree from a sequence
    let fromSeq size seq =
        let tree = create size
        for (pos,x) in seq do
            add pos x tree
        tree

    /// returns a chunk for a position
    let get (position:Vector2) stree =
        Dic.get (calcPos position stree) stree.Chunks

    /// visits all chunks that lies in the defined center position with
    /// width, height
    let inline getRec stree (center:Vector2) width height ([<InlineIfLambda>] f) =
        let struct (tl_x,tl_y) = calcPos (Vector2(center.X-width, center.Y-height)) stree
        let struct (tr_x,_   ) = calcPos (Vector2(center.X+width, center.Y-height)) stree
        let struct (_   ,bl_y) = calcPos (Vector2(center.X-width, center.Y+height)) stree
        for x=tl_x to tr_x do
            for y=tl_y to bl_y do
                match stree.Chunks.TryGetValue(struct (x,y)) with
                | false, _     -> ()
                | true , chunk -> for item in chunk do f item
