module Grids


type XY = (int * int)
let add: int -> int -> int = fun x y -> x + y

type Grid<'T when 'T: comparison> = Map<'T, XY>

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
            List.map (fun xy -> Map.tryFind xy grid) [ (x + 1, y); (x - 1, y); (x, y + 1); (x, y - 1) ]
            |> List.filter Option.isSome

        | FourDiagonal ->
            List.map (fun xy -> Map.tryFind xy grid) [ (x + 1, y + 1); (x - 1, y - 1); (x + 1, y - 1); (x - 1, y + 1) ]
            |> List.filter Option.isSome
        | Eight ->
            List.map
                (fun xy -> Map.tryFind xy grid)
                [ (x + 1, y)
                  (x - 1, y)
                  (x, y + 1)
                  (x, y - 1)
                  (x + 1, y + 1)
                  (x - 1, y - 1)
                  (x + 1, y - 1)
                  (x - 1, y + 1) ]
            |> List.filter Option.isSome
    //| _ ->  List.map(fun xy -> Map.find xy grid) [(1,1)]


    nbbs


let createGrid: string array -> (char -> bool) -> (char -> 'T) -> Grid<'T> =
    fun data accept conv ->
        let grid =
            data
            |> Array.mapi (fun r row ->
                row
                |> Seq.mapi (fun c ch -> (r, c, ch))
                |> Seq.filter (fun (_, _, ch) -> accept ch)
                |> Seq.map (fun (r, c, ch) -> (conv ch, (r, c))))
            |> Seq.concat
            |> Seq.fold (fun acc (ch, coord) -> Map.add ch coord acc) Map.empty

        grid
