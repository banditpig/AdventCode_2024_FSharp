module Grids

open System.Collections.Generic


type XY = (int * int)


type Grid<'T when 'T: comparison> = Map<XY, 'T>

type NeighbourType =
    | FourSquare
    | FourDiagonal
    | Eight


let neighboursAll grid nt (x, y) =

    let nbbs =
        match nt with
        | FourSquare -> [ (x + 1, y); (x - 1, y); (x, y + 1); (x, y - 1) ]

        | FourDiagonal -> [ (x + 1, y + 1); (x - 1, y - 1); (x + 1, y - 1); (x - 1, y + 1) ]

        | Eight ->
            [ (x + 1, y)
              (x - 1, y)
              (x, y + 1)
              (x, y - 1)
              (x + 1, y + 1)
              (x - 1, y - 1)
              (x + 1, y - 1)
              (x - 1, y + 1) ]


    nbbs


//let neighbours: Grid<'T> -> NeighbourType -> XY -> XY list =
let neighbours grid nt (x, y) =

    let nbbs =
        match nt with
        | FourSquare ->
            List.filter (fun xy -> Map.containsKey xy grid) [ (x + 1, y); (x - 1, y); (x, y + 1); (x, y - 1) ]

        | FourDiagonal ->
            List.filter
                (fun (a, b) -> Map.containsKey (a, b) grid)
                [ (x + 1, y + 1); (x - 1, y - 1); (x + 1, y - 1); (x - 1, y + 1) ]

        | Eight ->
            [ (x + 1, y)
              (x - 1, y)
              (x, y + 1)
              (x, y - 1)
              (x + 1, y + 1)
              (x - 1, y - 1)
              (x + 1, y - 1)
              (x - 1, y + 1) ]
            |> List.filter (fun (a, b) -> Map.containsKey (a, b) grid)

    nbbs

let findCoodsOf: 'T -> Grid<'T> -> Option<(XY) list> =
    fun t grid ->
        grid
        |> Map.filter (fun k v -> v = t)
        |> Map.keys
        |> List.ofSeq
        |> function
            | [] -> None
            | x -> Some x

let getItemAtXY (xy: XY) (grid: Grid<'T>) : 'T = Map.find xy grid

let uniqueValues (grid: Grid<'T>) : 'T list =
    grid |> Map.values |> Seq.distinct |> Seq.toList

let growFromSeed (seed) (startXY) (grid) (nt: NeighbourType) =

    let queue = Queue<XY>()
    let visited = HashSet<XY>()
    queue.Enqueue(startXY)
    let mutable borderLength = 0
    let mutable area = 1

    while queue.Count > 0 do

        let current = queue.Dequeue()

        let nbs = neighboursAll grid nt current
        let nbs' = List.filter (fun xy -> Map.tryFind xy grid = Some seed) nbs

        borderLength <- borderLength + List.length nbs - List.length nbs'
        visited.Add current |> ignore

        for n in nbs' do
            if not (visited.Contains n) then
                area <- area + 1
                queue.Enqueue n
                visited.Add n |> ignore


    (area, borderLength, visited)

let charToInt (c: char) : int = int c - int '0'

let createGrid (input: string) (conv: char -> 'T) : Grid<'T> =

    input.Split([| '\n' |], System.StringSplitOptions.RemoveEmptyEntries)
    |> Array.mapi (fun i line -> line.ToCharArray() |> Array.mapi (fun j c -> ((i, j), conv c)))
    |> Array.concat
    |> Map.ofArray
