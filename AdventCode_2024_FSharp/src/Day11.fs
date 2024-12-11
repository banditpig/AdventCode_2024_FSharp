module Day11

open System.Collections.Generic

let data = "125 17"
let mapping = Dictionary<int, int>()

let addOrUpdate (key: int) =
    if mapping.ContainsKey key then
        mapping.[key] <- mapping.[key] + 1
    else
        mapping.Add(key, 1)

let splitBrick (brick: string) : string list =
    let len = brick.Length
    let half = len / 2

    [ brick.Substring(0, half).TrimStart('0').PadLeft(1, '0')
      brick.Substring(half, half).TrimStart('0').PadLeft(1, '0') ]

let processBrick (brick: string) : string list =

    let r =
        match brick with
        | "0" -> [ "1" ]
        | _ when brick.Length % 2 = 0 -> splitBrick brick
        | _ -> [ int64 brick * 2024L |> string ]

    r

let processBricks (bricks: string list) =
    bricks
    |> List.map processBrick
    // |> Array.concat
    // |> Li.toList
    |> List.concat

let part1 () =
    let mutable bricks = data.Split(' ') |> Array.toList

    for i = 0 to 25 do

        bricks <- processBricks bricks


    // processBricks ["125"; "17"; "0"] |> printfn "%A"
    // processBrick ("125") |> printfn "%A"
    // processBrick ("0") |> printfn "%A"
    // processBrick ("17") |> printfn "%A"

    printfn $"Day11 Part1 %A{bricks.Length}"

    ()
