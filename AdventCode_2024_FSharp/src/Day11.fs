module Day11

open System.Collections.Generic

let data = "4329 385 0 1444386 600463 19 1 56615"
let memo = Dictionary<string, string list>()


let splitBrick (brick: string) : string list =
    let len = brick.Length
    let half = len / 2

    [ brick.Substring(0, half).TrimStart('0').PadLeft(1, '0')
      brick.Substring(half, half).TrimStart('0').PadLeft(1, '0') ]

let processBrick' (brick: string) : string list =
    match memo.TryGetValue brick with
    | true, result -> result
    | false, _ ->
        let result =
            match brick with
            | "0" -> [ "1" ]
            | _ when brick.Length % 2 = 0 -> splitBrick brick
            | _ -> [ int64 brick * 2024L |> string ]

        memo.Add(brick, result)
        result

let processBrick (brick: string) : string list =

    let r =
        match brick with
        | "0" -> [ "1" ]
        | _ when brick.Length % 2 = 0 -> splitBrick brick
        | _ -> [ int64 brick * 2024L |> string ]

    r

let processBricks (bricks: string list) =
    bricks
    |> List.map processBrick'
    // |> Array.concat
    // |> Li.toList
    |> List.concat

let splitNumber n =
    let digits = n.ToString().Length
    let half = digits / 2
    let left = n / pown 10 half
    let right = n % pown 10 half
    (left, right)

let processStone stone =
    match stone with
    | 0 -> [ 1 ] // Rule 1: 0 becomes 1
    | _ when stone.ToString().Length % 2 = 0 ->
        // Rule 2: Split into two stones
        let left, right = splitNumber stone
        [ left; right ]
    | _ ->
        // Rule 3: Multiply by 2024
        [ stone * 2024 ]

let blink stones = stones |> List.collect processStone

let simulateBlinks initialStones blinks =
    let rec loop currentStones remainingBlinks =
        if remainingBlinks = 0 then
            currentStones
        else
            let nextStones = blink currentStones
            loop nextStones (remainingBlinks - 1)

    loop initialStones blinks |> List.length

let part1 () =
    let mutable bricks = data.Split(' ') |> Array.toList

    for i = 0 to 74 do
        bricks <- processBricks bricks

    printfn $"Day11 Part1 %A{bricks.Length}"

    ()

let part2 () =
    // 125 17  - 65601038650482
    //4329 385 0 1444386 600463 19 1 56615
    let initialStones = [ 4329; 385; 0; 1444386; 600463; 19; 1; 56615 ]
    let blinks = 25 //218079
    let result = simulateBlinks initialStones blinks
    printfn "Number of stones after %d blinks: %d" blinks result
