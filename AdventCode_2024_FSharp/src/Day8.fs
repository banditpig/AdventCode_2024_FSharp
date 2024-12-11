module Day8




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


//list comprehension all pairs of coordinates
let pairs (lst: 'a list) =
    [ for i in 0 .. lst.Length - 1 do
          for j in i + 1 .. lst.Length - 1 do
              yield lst.[i], lst.[j] ]

//let data = File.ReadAllLines(filePath)
let part1 () =
    // let data = File.ReadAllLines("./Data/Day8.txt")
    // let grid = Grids.createGrid data (fun ch -> ch <> '.') id
    // printfn "Day8 Part1 %A"

    // let x =
    //     grid
    //     |> Map.map (fun ch coords -> pairs coords)
    //     |> Map.map (fun ch pairs -> pairs |> List.map (fun (c1, c2) -> pointsOnLineWithinGrid c1 c2 (12, 12)))
    //if r,c is
    printfn "Day8 Part1"
