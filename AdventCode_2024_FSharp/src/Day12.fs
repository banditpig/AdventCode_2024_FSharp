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
let part1' () =

    let data = System.IO.File.ReadAllText "./Data/Day12.txt"
    let g: Grid<char> = Grids.createGrid data id


    let mutable total = 0
    let mutable seen = HashSet<XY>()

    for c in g.Values do
        let xys = Grids.findCoodsOf c g |> Option.get

        for xy in xys do
            if not (seen.Contains xy) then
                do
                    let (a, p, checked) = Grids.growFromSeed c xy g FourSquare
                    total <- total + (a * p)
                    checked |> Seq.iter (fun xy -> seen.Add xy |> ignore)
                    checked.Add xy |> ignore

    printfn "Day 12 Part 1 %A" total

let part1 () =
    let data = System.IO.File.ReadAllText "./Data/Day12.txt"
    let g: Grid<char> = Grids.createGrid data id

    let seen = HashSet<XY>()

    let total =
        g.Values
        |> Seq.collect (fun c -> Grids.findCoodsOf c g |> Option.toList)
        |> Seq.collect id
        |> Seq.filter (fun xy -> not (seen.Contains xy))
        |> Seq.sumBy (fun xy ->
            let c = g.[xy]
            let (a, p, checked) = Grids.growFromSeed c xy g FourSquare
            checked |> Seq.iter (fun xy -> seen.Add xy |> ignore)
            seen.Add xy |> ignore
            a * p)

    printfn $"Day 12 Part 1 %A{total}"
