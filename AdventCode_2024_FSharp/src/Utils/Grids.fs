module Grids


type XY = (int * int)
let add: int -> int -> int = fun x y -> x + y

type Grid<'T when 'T: comparison> = Map<XY, 'T>

type NeighbourType =
    | FourSquare
    | FourDiagonal
    | Eight

// let tryGet: XY -> Grid<'T > -> Option<'T> =
//     fun xy grid -> Map.tryFind xy grid



let findInGrid (t: 'T) (grid: Grid<'T>) : Option<XY> = Map.tryFind t grid // This works if Grid is a proper alias for Map

//let neighbours: Grid<'T> -> NeighbourType -> XY -> XY list =
let neighbours grid nt (x, y) =

    let nbbs =
        match nt with
        | FourSquare ->
            let nbs =
                List.filter (fun xy -> Map.containsKey xy grid) [ (x + 1, y); (x - 1, y); (x, y + 1); (x, y - 1) ]


            nbs
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

    //| _ ->  List.map(fun xy -> Map.find xy grid) [(1,1)]


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

let convertToMap (input: string) (conv: char -> 'T) : Grid<'T> =
    let x = input.Split([| '\n' |], System.StringSplitOptions.RemoveEmptyEntries)

    x
    |> Array.mapi (fun i line -> line.ToCharArray() |> Array.mapi (fun j c -> ((i, j), conv c)))
    |> Array.concat
    |> Map.ofArray
// let createGrid:  string   -> (char -> bool) -> (char -> 'T) -> Grid<'T> =
//     fun data accept conv ->
//          data.Split([|'\n'|], System.StringSplitOptions.RemoveEmptyEntries)
//             |> Array.mapi (fun i line ->  line.ToCharArray())
//             |> Array.mapi (fun j c -> ((i, j), conv  c))
//             |> Array.concat
//             |> Map.ofArray

// let mutable grid = Map.empty
//
// for j in 0 .. data.Length - 1 do
//     for i in 0 .. data.[0].Length - 1 do
//         let xx = data.[j].[i]
//
//         printfn $"({j}, {i}) {xx} "
//
//         if accept xx then
//             let xx' = conv xx
//
//             grid <- Map.add (j, i) (xx') grid

//grid

// fun data accept conv ->
//     let grid =
//         data
//         |> Array.mapi (fun r row ->
//             row
//             |> Seq.mapi (fun c ch -> (r, c, ch))
//             |> Seq.filter (fun (_, _, ch) -> accept ch)
//             |> Seq.map (fun (r, c, ch) -> (conv ch, (r, c))))
//         |> Seq.concat
//         |> Seq.fold (fun acc (ch, coord) -> Map.add ch coord acc) Map.empty
//
//     grid
