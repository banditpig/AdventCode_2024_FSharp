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
    { mutable direction: Direction
      mutable currentLocation: XY
      exitLocation: XY
      mutable exited: bool
      mutable trail: XY Set }

type FloorMap = Cell[,]

let nextCell (xy: XY, d: Direction, map: FloorMap) : Option<Cell> =

    match d with
    | Left ->
        match xy.x with
        | 0 -> None
        | _ -> Some map.[xy.y, xy.x - 1]

    | Right ->
        match xy.x with
        | _ when xy.x >= map.GetLength(1) -> None
        | _ -> Some map.[xy.y, xy.x + 1]

    | Up ->
        match xy.y with
        | 0 -> None
        | _ -> Some map.[xy.y - 1, xy.x]

    | Down ->
        match xy.y with
        | _ when xy.y >= map.GetLength(0) -> None
        | _ -> Some map.[xy.y + 1, xy.x]




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

let rec moveGuard (guard: Guard, map: FloorMap) : Guard =
    //printfn "Guard %A" <| Set.count guard.trail
    let next = nextCell (guard.currentLocation, guard.direction, map)

    match next with
    | None -> guard
    | Some cell ->
        match cell with
        | Empty ->
            let nextLocation = nextXY (guard.direction, guard.currentLocation)

            guard.currentLocation <- nextLocation
            guard.trail <- Set.add nextLocation guard.trail
            moveGuard (guard, map)
        | Obstacle ->
            guard.direction <- rotateRight guard.direction
            moveGuard (guard, map)




let fillMap (filePath: string) =

    let lines = File.ReadAllLines(filePath)
    let height = lines.Length
    let width = lines.[0].Length
    let mutable map = Array2D.init<Cell> height width (fun _ _ -> Empty)

    let mutable guard =
        { direction = Right
          currentLocation = { x = 0; y = 0 }
          exitLocation = { x = width - 1; y = height - 1 }
          exited = false
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
                          currentLocation = { x = x; y = y }
                          exitLocation = { x = width - 1; y = height - 1 }
                          exited = false
                          trail = Set.empty }

                    Empty
                | _ -> Empty

            map.[y, x] <- cellType

    (map, guard)

let evaluateMap (map: FloorMap) (guard: Guard) : Guard =
    let newGuard = moveGuard (guard, map)
    newGuard




let part1 () =
    //4453 too low answer is 4454
    let (map, guard) = fillMap "./Data/Day6.txt" // let map = fillMap "./Data/Day6.txt"
    let g = evaluateMap map guard
    printfn "Day6 Part1 %A" <| Set.count g.trail + 1
