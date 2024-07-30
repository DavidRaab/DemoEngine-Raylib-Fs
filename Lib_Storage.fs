namespace MyGame

module ResizeArray =
    /// Binary Search for an ResizeArray
    let bsearch (value:'a) (array:ResizeArray<'a>) : voption<int> =
        let rec findRec lpos rpos =
            if lpos > rpos then
                ValueNone
            else
                let mid   = lpos + ((rpos - lpos) / 2)
                let check = array.[mid]
                if value < check then
                    findRec lpos (mid-1)
                else if value > check then
                    findRec (mid+1) rpos
                else
                    ValueSome mid
        findRec 0 (array.Count-1)

module Storage =
    type T<'Key,'Value> = {
        Keys:   ResizeArray<'Key>
        Values: ResizeArray<'Value>
    }

    let create<'Key,'Value> () = {
        Keys   = ResizeArray<'Key>()
        Values = ResizeArray<'Value>()
    }

    /// returns the amount of elements stored
    let length storage =
        storage.Keys.Count

    /// Checks if a key exists and returns the index if present
    let containsKey key storage : voption<int> =
        ResizeArray.bsearch key storage.Keys

    /// Swaps to fields of an ResizeArray
    let inline private swap x y (data:ResizeArray<'a>) : unit =
        let tmp = data.[x]
        data.[x] <- data.[y]
        data.[y] <- tmp

    /// inserts a key with a given value into the storage. When key
    /// already exists, then value is overwritten. When key exists
    /// has O(log n) running time, otherwise O(n).
    let insert key value storage =
        match containsKey key storage with
        | ValueSome idx -> storage.Values.[idx] <- value
        | ValueNone     ->
            let keys = storage.Keys
            let vals = storage.Values
            keys.Add key
            vals.Add value

            let mutable idx     = storage.Keys.Count - 1
            let mutable running = true
            while running do
                if idx > 0 && keys.[idx-1] > keys.[idx] then
                    swap (idx-1) idx keys
                    swap (idx-1) idx vals
                    idx <- idx - 1
                else
                    running <- false

    /// Removes a key from Storage. Has O(n) running time. If key does not
    /// exists then does nothing.
    let remove key storage : unit =
        match containsKey key storage with
        | ValueNone     -> ()
        | ValueSome idx ->
            let keys = storage.Keys
            let vals = storage.Values
            let last = keys.Count - 1
            for i=idx to last-1 do
                swap i (i+1) keys
                swap i (i+1) vals
            keys.RemoveAt(last)
            vals.RemoveAt(last)

    /// Iterates through an storage with key and value
    let inline iter ([<InlineIfLambda>] f) storage : unit =
        let keys = storage.Keys
        let vals = storage.Values
        let last = keys.Count - 1
        for idx=0 to last do
            f keys.[idx] vals.[idx]