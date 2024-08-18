namespace Storage

type Dictionary<'a,'b> = System.Collections.Generic.Dictionary<'a,'b>

module Storage =
    type T<'Key,'Value> = {
        KeyToIndex: Dictionary<'Key,int>
        Data:       ResizeArray<struct('Key * 'Value)>
    }

    let create<'Key,'Value when 'Key:equality> () : T<'Key,'Value> = {
        KeyToIndex = Dictionary<'Key,int>()
        Data       = ResizeArray<struct('Key * 'Value)>()
    }

    let length storage = storage.Data.Count
    let count  storage = storage.Data.Count

    let contains key (storage:T<'Key,'value>) : bool =
        if storage.KeyToIndex.ContainsKey(key) then true else false

    /// returns 'Value for 'Key
    let get (key:'Key) storage : voption<'Value> =
        match storage.KeyToIndex.TryGetValue(key) with
        | true, idx ->
            let struct (_,value) = storage.Data.[idx]
            ValueSome value
        | false, _  -> ValueNone

    /// returns internal index for 'Key. Should not be cached as it can become invalid
    /// This function should be used when there is an `get` followed by an `add`
    /// operation. You get the index, with the index you can fetch the entry from
    /// the data field and are able to overwrite the entry. Usually only useful
    /// when data-field is a struct or an immutable type.
    let inline getIndex (key:'Key) storage : voption<int> =
        match storage.KeyToIndex.TryGetValue(key) with
        | true, idx -> ValueSome idx
        | false, _  -> ValueNone

    /// Overwrites value at position that was returned by `getIndex`.
    let saveAt (index:int) (value:'Value) storage : unit =
        let struct (key,_) = storage.Data.[index]
        storage.Data.[index] <- struct (key,value)

    /// adds a key,value mapping. Either newly inserting or overwriting existing data
    let add (key:'Key) (value:'Value) storage : unit =
        match getIndex key storage with
        | ValueSome idx -> storage.Data.[idx] <- struct (key,value)
        | ValueNone     ->
            storage.Data.Add(struct(key,value))
            let last = storage.Data.Count - 1
            storage.KeyToIndex.Add(key,last)

    let remove (key:'Key) storage : unit =
        match getIndex key storage with
        | ValueSome idx ->
            // we can efficently remove any key key by just swaping it with the last
            // element, and remove last element. I hope RemoveAt() just decrease
            // internal size by -1 and sets last element to null.
            let lastIdx = storage.Data.Count - 1
            if idx = lastIdx then
                storage.Data.RemoveAt(lastIdx)
                storage.KeyToIndex.Remove(key) |> ignore
            else
                // overwrite key to delete with last element and delete
                // key -> index mapping.
                storage.Data.[idx] <- storage.Data.[lastIdx]
                storage.KeyToIndex.Remove(key) |> ignore

                // Now key,value mapping doesn't exist anymore, but we need to
                // remove last element so last element doesn't appear twice
                storage.Data.RemoveAt(lastIdx)

                // Now we need to update key -> index for swaped element
                let struct (key,_) = storage.Data.[idx]
                storage.KeyToIndex.[key] <- idx
        | ValueNone ->
            ()

    /// get an element by key and executes a function for side-effects
    let inline fetch f key storage =
        match getIndex key storage with
        | ValueSome idx -> (f storage.Data.[idx])
        | ValueNone     -> ()

    /// Itereates the data-structure in random order and executes a function
    /// for each key,value pair
    let inline iter ([<InlineIfLambda>] f) storage =
        let mutable idx = 0
        for (key,value) in storage.Data do
            f key value
            idx <- idx + 1