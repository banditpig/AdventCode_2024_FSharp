module Arrays

let swapArrayElements (arr: 'T[]) index1 index2 =
    if index1 <> index2 then
        let temp = arr.[index1]
        arr.[index1] <- arr.[index2]
        arr.[index2] <- temp

let lastIndexThatSatisfies (arr: 'T[]) (f: 'T -> bool) =
    arr
    |> Array.mapi (fun i x -> i, x) // Map indices to values
    |> Array.filter (fun (_, x) -> f x) // Keep only matching elements
    |> Array.map fst // Extract indices
    |> Array.tryLast // Get the last index if it exists
