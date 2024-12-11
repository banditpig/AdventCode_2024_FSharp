module Grids


type XY = (int * int)
let add: int -> int -> int = fun x y -> x + y

type Grid<'T when 'T: comparison> = Map<XY, 'T>

type NeighbourType =
    | FourSquare
    | FourDiagonal
    | Eight


let findInGrid (t: 'T) (grid: Grid<'T>) : Option<XY> = Map.tryFind t grid // This works if Grid is a proper alias for Map

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



let charToInt (c: char) : int = int c - int '0'

let createGrid (input: string) (conv: char -> 'T) : Grid<'T> =

    input.Split([| '\n' |], System.StringSplitOptions.RemoveEmptyEntries)
    |> Array.mapi (fun i line -> line.ToCharArray() |> Array.mapi (fun j c -> ((i, j), conv c)))
    |> Array.concat
    |> Map.ofArray
