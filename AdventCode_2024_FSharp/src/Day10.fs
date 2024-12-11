module Day10

open System.Collections.Generic
let bfsFindAllPaths (grid: Grids.Grid<int>) (start: 'a) (target: 'a)  : 'a list list =
    // Queue to store paths being explored
    let queue = Queue<'a list>()
    queue.Enqueue([start]) // Start with a path containing only the starting point

    let mutable result = []

    while queue.Count > 0 do
        // Dequeue the next path to explore
        let currentPath = queue.Dequeue()
        let currentNode = List.last currentPath

        if currentNode = target then
            // If the target is reached, add the path to the result
            result <- currentPath :: result
        else
            // Explore neighbors that are not already in the current path
            let h = Map.find currentNode grid
                
            let neighbours = Grids.neighbours grid Grids.NeighbourType.FourSquare currentNode
            let nbs = List.filter (fun x -> (Map.find x grid) = h + 1) neighbours

            nbs
            |> List.filter (fun neighbor -> not (List.contains neighbor currentPath))
            |> List.iter (fun neighbor -> queue.Enqueue(currentPath @ [neighbor]))

    result // Return all found paths
let path start (grid: Grids.Grid<int>) : int =
    //bfs search for path that ned with 9

    let visited = HashSet<Grids.XY>()
    let queue = Queue<Grids.XY>()
    let nines = HashSet<Grids.XY>()
    let mutable nineCount = 0
    queue.Enqueue start
    visited.Add start |> ignore

    while queue.Count > 0 do
        let current = queue.Dequeue()
        let h = Map.find current grid
        let neighbours = Grids.neighbours grid Grids.NeighbourType.FourSquare current
        let nbs = List.filter (fun x -> (Map.find x grid) = h + 1) neighbours

        for n in nbs do
            if not (visited.Contains n) then
                queue.Enqueue n

                if (Map.find n grid) = 9 then
                    nineCount <- nineCount + 1
                    nines.Add n |> ignore

                visited.Add n |> ignore




    nineCount





// let nbs = Grids.neighbours grid Grids.FourSquare current
//      let nbs' = List.filter (fun x -> (Map.find x grid) = h + 1) nbs
//



let part1 () =




    // convert  data into a 2d array of char


    let data = System.IO.File.ReadAllText "./Data/Day10.txt"
    let grid = Grids.convertToMap (data) (Grids.charToInt)

    let starts = Grids.findCoodsOf 0 grid
    let ends = Grids.findCoodsOf 9 grid
    let mutable x = 0

    for start in starts.Value do
        x <- x + (path (start) grid)




    printfn "Day10 Part1 %A " x
