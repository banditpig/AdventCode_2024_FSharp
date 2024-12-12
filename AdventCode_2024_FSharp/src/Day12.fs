module Day12

open System.Collections.Generic
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
// let data = "AAAA
// BBBB
// ACCA
// DDDD"
let part1 () =

    let data = System.IO.File.ReadAllText "./Data/Day12.txt"
    let g: Grid<char> = Grids.createGrid data id


    let mutable total = 0
    let mutable seen = HashSet<XY>()

    for c in g.Values do
        let xys = Grids.findCoodsOf c g |> Option.get

        for xy in xys do
            if not (seen.Contains xy) then
                do
                    //let xy = Grids.findCoodsOf c g |> Option.get |> List.head
                    let (a, p, checked) = Grids.growFromSeed c xy g FourSquare
                    // printfn "Char %A Area %A Perimeter %A" c a p
                    total <- total + (a * p)
                    checked |> Seq.iter (fun xy -> seen.Add xy |> ignore)
                    checked.Add xy |> ignore


    let ab = Grids.growFromSeed 'A' (0, 2) g FourSquare

    printfn "Day 12 Part 1 %A" total
    0
