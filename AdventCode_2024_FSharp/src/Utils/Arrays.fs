module Arrays


let swapArrayElements (arr: 'T[]) index1 index2 =
    if index1 <> index2 then
        let temp = arr.[index1]
        arr.[index1] <- arr.[index2]
        arr.[index2] <- temp

let contiguousBlockOf (c: 'T) (from: int) (arr: 'T[]) : Option<int * int> =
    if from >= Array.length arr then
        None
    else
        // Find the start of the block
        match Array.tryFindIndex (fun x -> x = c) (Array.skip from arr) with
        | None -> None
        | Some startOffset ->
            let startIndex = from + startOffset
            // Find the end of the block
            match Array.tryFindIndex (fun x -> x <> c) (Array.skip (startIndex + 1) arr) with
            | None -> Some(startIndex, Array.length arr - 1) // Block extends to the end
            | Some endOffset -> Some(startIndex, startIndex + endOffset)

let lastIndexThatSatisfies (arr: 'T[]) (f: 'T -> bool) =
    arr
    |> Array.mapi (fun i x -> i, x) // Map indices to values
    |> Array.filter (fun (_, x) -> f x) // Keep only matching elements
    |> Array.map fst // Extract indices
    |> Array.tryLast // Get the last index if it exists
