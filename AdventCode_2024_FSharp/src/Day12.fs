module Day12

open Grids

let data =
    "RRRRIICCFF
RRRRIICCCF
VVRRRCCFFF
VVRCCCJFFF
VVVVCJJCFE
VVIVCCJJEE
VVIIICJJEE
MIIIIIJJEE
MIIISIJEEE
MMMISSJEEE"

let part1 () =

    let g: Grid<char> = Grids.createGrid data id
    let unique = Grids.uniqueValues g
    let mutable area = 0
    let mutable perimeter = 0
    let mutable total = 0

    for c in unique do
        let xy = Grids.findCoodsOf c g |> Option.get |> List.head
        let (a, p) = Grids.growFromSeed c xy g FourSquare
        printfn "Char %A Area %A Perimeter %A" c a p
        total <- total + (a * p)


    let ab = Grids.growFromSeed 'A' (0, 2) g FourSquare

    printfn "Day 12 Part 1 %A" total
    0
