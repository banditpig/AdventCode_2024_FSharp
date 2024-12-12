module Day12

open Grids

let data =
    "AAAA
BBCD
BBCC
EEEC"

let part1 () =
    let g: Grid<char> = Grids.createGrid data id
    let ab = Grids.growFromSeed 'A' (0, 2) g FourSquare

    printfn "Day 12 Part 1 %A" ab
    0
