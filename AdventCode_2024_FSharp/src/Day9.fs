module Day9

type Entry =
    | BlockFile
    | Space

let flipEntry (e: Entry) : Entry =
    match e with
    | BlockFile -> Space
    | Space -> BlockFile

let incId (id: int) (e: Entry) : int =
    match e with
    | BlockFile -> id + 1
    | Space -> id

let intToChar n = char (n + int '0')
let charToInt c = int c - int '0'
let charToInt64 = charToInt >> int64



let checkSum (data: char array) : int64 =
    data
    |> Array.filter (fun x -> x <> '.')
    |> Array.mapi (fun i x -> int64 i * ((charToInt64 x)))
    |> Array.sum

let restructure (data: char list) =

    let dataArray = data |> List.toArray

    let mutable ok = false

    while ok = false do
        let ixEmpty = Array.findIndex (fun x -> x = '.') dataArray
        let ixFull = Arrays.lastIndexThatSatisfies dataArray (fun x -> x <> '.')

        match ixFull with
        | Some ixFull ->
            ok <- ixFull < ixEmpty

            if (ok = false) then
                Arrays.swapArrayElements dataArray ixEmpty ixFull
        | None -> ok <- true

    dataArray

let rec expand (d: char list) : char list =

    let rec expand' (id: int) entry (data: char list) (result: char list) : char list =

        let g (i: int) (entry: Entry) (theChar: char) =
            match entry with
            | Space -> List.replicate (charToInt theChar) '.'
            | BlockFile -> List.replicate (charToInt theChar) (intToChar i)

        match data with
        | [] -> result // Handle empty string
        | x :: xs -> expand' (incId id entry) (flipEntry entry) xs (result @ g id entry x)

    expand' 0 BlockFile d []


let part1 () =
    //let data = File.ReadAllText "./Data/Day9.txt" |> Seq.toList
    let data = "2333133121414131402" |> Seq.toList
    let expanded = expand data
    printfn "Day 9 Part 1 %A" (new string (expanded |> List.toArray))
    let restructured = restructure expanded
    printfn "Day 9 Part 1 %A" (checkSum restructured)


    //printfn "Day 9 Part 1 %A" <| new string (restructure expanded) // (new string (expand data |> List.toArray))
    0
