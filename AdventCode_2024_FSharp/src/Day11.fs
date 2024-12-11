module Day11

let data = "4329 385 0 1444386 600463 19 1 56615"

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

    for i = 0 to 5 do

        bricks <- processBricks bricks


    // processBricks ["125"; "17"; "0"] |> printfn "%A"
    // processBrick ("125") |> printfn "%A"
    // processBrick ("0") |> printfn "%A"
    // processBrick ("17") |> printfn "%A"

    printfn $"Day11 Part1 %A{bricks.Length}"

    ()
