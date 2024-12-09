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
let swapArrayElements (arr: 'T[]) index1 index2 =
    if index1 <> index2 then
        let temp = arr.[index1]
        arr.[index1] <- arr.[index2]
        arr.[index2] <- temp
        
let restructure (data: char list) : char list =
   //get first empty element a '.'
   //get the last  full element  a digit
   //swap them
   //repeat until no more empty
   
   data
    
let rec expand (d: char list) : char list =

    let rec expand' (id: int) entry (data: char list) (result: char list) : char list =

        let g (i: int) (entry: Entry) (theChar: char) =
            match entry with
            | Space -> List.replicate (charToInt theChar) '.'
            | BlockFile -> List.replicate (charToInt theChar) (intToChar id)

        match data with
        | [] -> result // Handle empty string
        | x :: xs -> expand' (incId id entry) (flipEntry entry) xs (result @ g id entry x)

    expand' 0 BlockFile d []


let part1 () =
    let data = "2333133121414131402" |> Seq.toList

    printfn "Day 9 Part 1 %A" (new string (expand data |> List.toArray))
    0
