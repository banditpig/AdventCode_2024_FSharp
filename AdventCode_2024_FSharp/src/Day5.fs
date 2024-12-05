module Day5

open System.IO

let loadRules (filePath: string) : string list =
    File.ReadAllLines(filePath) |> List.ofArray

let loadUpdates (filePath: string) =
    File.ReadAllLines(filePath)
    |> List.ofArray
    |> List.map (fun x -> x.Split(",") |> List.ofArray)

let rowOk rules (row, pairs) =
    //all pairs of rules are in rules
    pairs |> List.forall (fun p -> List.contains p rules)

let pairs r =
    [ for i in 0 .. List.length r - 1 do
          for j in i + 1 .. List.length r - 1 do
              yield $"{r.[i]}|{r.[j]}" ]

let applyRules rules updates =
    let cnt =
        updates
        |> List.map (fun row -> (row, pairs row))
        |> List.filter (rowOk rules)
        |> List.map (fun (row, _) -> row)
        |> List.map (fun r -> r.[(List.length r) / 2])
        |> List.map (fun r -> int r)
        |> List.sum

    cnt


let sortUpdates rules updates = updates



let failedUpdates rules updates =
    let bad =
        updates
        |> List.map (fun row -> (row, pairs row))
        |> List.filter (fun x -> not <| rowOk rules x)
        |> List.map (fun (row, _) -> row)

    bad


let part1 () =
    let rules = loadRules "./Data/Day5Rules.txt"
    let updates = loadUpdates "./Data/Day5Updates.txt"
    let cnt = applyRules rules updates
    printfn "Day5 Part1  %A" cnt

let part2 () =
    let rules = loadRules "./Data/Day5Rules.txt"
    let updates = loadUpdates "./Data/Day5Updates.txt"
    let failedUpdates = failedUpdates rules updates
    sortUpdates rules failedUpdates |> applyRules rules |> printfn "Day5 Part2 %A "
