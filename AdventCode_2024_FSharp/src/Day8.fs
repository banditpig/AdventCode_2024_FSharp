module Day8

open System.IO



// Function to compute gcd
let rec gcd a b = if b = 0 then abs a else gcd b (a % b)

// Function to generate all grid points on the line
let pointsOnLineWithinGrid (x1, y1) (x2, y2) (xmax, ymax) =
    let dx = x2 - x1
    let dy = y2 - y1
    let g = gcd dx dy
    let stepX = dx / g
    let stepY = dy / g

    // Generate points in both positive and negative directions
    let rec generatePoints x y direction acc =
        if x < 0 || x > xmax || y < 0 || y > ymax then
            acc
        else
            generatePoints (x + direction * stepX) (y + direction * stepY) direction ((x, y) :: acc)

    let positivePoints = generatePoints x1 y1 1 []
    let negativePoints = generatePoints x1 y1 -1 []

    // Combine points and remove duplicates (optional for symmetrical line)
    positivePoints @ negativePoints |> List.distinct


let distance (r1, c1) (r2, c2) = abs (r1 - r2) + abs (c1 - c2)

let createGrid: Map<char, (int * int) list> =
    let data = File.ReadAllLines("./Data/Day8.txt")

    let grid =
        data
        |> Array.mapi (fun r row -> //rows
            row
            |> Seq.mapi (fun c ch -> (r, c, ch)) //columns
            |> Seq.filter (fun (_, _, ch) -> ch <> '.') //filter out empty cells
            |> Seq.map (fun (r, c, ch) -> (ch, (r, c)))) //map to (ch, (r, c))
        |> Seq.concat //add all rows to one sequence
        |> Seq.fold // fold over sequence
            (fun acc (ch, coord) -> //acc is the accumulator
                match Map.tryFind ch acc with //do we have an existing list for this character?
                | Some coordList -> Map.add ch (coordList @ [ coord ]) acc //add to existing list
                | None -> Map.add ch [ coord ] acc) //add new list
            Map.empty //start with empty map

    grid


//list comprehension all pairs of coordinates
let pairs (lst: 'a list) =
    [ for i in 0 .. lst.Length - 1 do
          for j in i + 1 .. lst.Length - 1 do
              yield lst.[i], lst.[j] ]


let part1 () =
    let grid = createGrid
    printfn "Day8 Part1 %A" 
    let x =grid |> Map.map (fun ch coords -> pairs coords)
         |> Map.map (fun ch pairs -> pairs |> List.map (fun (c1, c2) -> pointsOnLineWithinGrid  c1 c2 (12 ,12)))
    //if r,c is
    printfn "Day8 Part1 %A" x
