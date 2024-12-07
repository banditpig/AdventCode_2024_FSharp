module Day6

open System.IO

type Direction =
    | Left
    | Right
    | Up
    | Down

type XY = { x: int; y: int }

type Cell =
    | Empty
    | Obstacle
// | Guard of Direction * location: XY * trail: Set<XY>



type Guard =
    { direction: Direction
      currentLocation: XY
      cycle: bool
      startLocation: XY

      trail: Set<Direction * XY> }

type FloorMap = Cell[,]

let nextCell (xy: XY, d: Direction, map: FloorMap) : Option<Cell> =

    match d with
    | Left ->
        match xy.x with
        | 0 -> None
        | _ -> Some map.[xy.y, xy.x - 1]

    | Right ->
        match xy.x with
        | _ when xy.x < map.GetLength(0) - 1 -> Some map.[xy.y, xy.x + 1]
        | _ -> None

    | Up ->
        match xy.y with
        | 0 -> None
        | _ -> Some map.[xy.y - 1, xy.x]

    | Down ->
        match xy.y with
        | _ when xy.y < map.GetLength(1) - 1 -> Some map.[xy.y + 1, xy.x]
        | _ -> None




let rotateRight (d: Direction) : Direction =
    match d with
    | Left -> Up
    | Right -> Down
    | Up -> Right
    | Down -> Left

let nextXY (d: Direction, xy: XY) : XY =
    match d with
    | Left -> { xy with x = xy.x - 1 }
    | Right -> { xy with x = xy.x + 1 }
    | Up -> { xy with y = xy.y - 1 }
    | Down -> { xy with y = xy.y + 1 }

let exitMap (xy: XY) (map: FloorMap) : bool =
    if xy.x < 0 || xy.y < 0 then
        true
    elif xy.x >= map.GetLength(1) || xy.y >= map.GetLength(0) then
        true
    else
        false

let rec moveGuard (map: FloorMap) (guard: Guard) : Guard =

    let next = nextCell (guard.currentLocation, guard.direction, map)

    match next with
    | None -> guard
    | Some Empty ->
        let nextLocation = nextXY (guard.direction, guard.currentLocation)

        if Set.contains (guard.direction, guard.currentLocation) guard.trail then
            printfn "Cycle detected"
            { guard with cycle = true }
        else
            { guard with
                trail = Set.add (guard.direction, guard.currentLocation) guard.trail
                currentLocation = nextLocation }
            |> moveGuard map
    | Some Obstacle ->

        { guard with
            direction = rotateRight guard.direction }
        |> moveGuard map

let fillMap (filePath: string) =

    let lines = File.ReadAllLines(filePath)
    let height = lines.Length
    let width = lines.[0].Length
    let mutable map = Array2D.init<Cell> height width (fun _ _ -> Empty)

    let mutable guard =
        { direction = Right
          cycle = false
          startLocation = { x = 0; y = 0 }
          currentLocation = { x = 0; y = 0 }
          trail = Set.empty }

    for y in 0 .. height - 1 do
        for x in 0 .. width - 1 do
            let cell = lines.[y].[x]

            let cellType =
                match cell with
                | '#' -> Obstacle
                | '^' ->
                    guard <-
                        { direction = Up
                          cycle = false
                          startLocation = { x = x; y = y }
                          currentLocation = { x = x; y = y }
                          trail = Set.empty }

                    Empty
                | _ -> Empty

            map.[y, x] <- cellType

    (map, guard)

let evaluateMap (map: FloorMap) (guard: Guard) : Guard = moveGuard map guard




let part1 () =

    let (map, guard) = fillMap "./Data/Day6.txt" // let map = fillMap "./Data/Day6.txt"
    let g = evaluateMap map guard
    printfn "Day6 Part1 %A" <| Set.count g.trail

let part2 () =
    //1503 is correct
    let map, guard = fillMap "./Data/Day6.txt"
    let mutable cycleCount = 0

    for row in 0 .. Array2D.length1 map - 1 do
        for col in 0 .. Array2D.length2 map - 1 do
            if
                map.[row, col] = Empty
                && (row <> guard.startLocation.y || col <> guard.startLocation.x)
            then
                // Create a temporary copy of the map
                let testMap = Array2D.copy map
                testMap.[row, col] <- Obstacle

                let result = moveGuard testMap guard

                // Check if a cycle was created
                if result.cycle then
                    cycleCount <- cycleCount + 1

    printfn "Day6 Part2 Cycle Count: %d" cycleCount

// let g = evaluateMap map guard 1448, 1449
// printfn "Day6 Part1 %A" <| Set.count g.trail + 1
