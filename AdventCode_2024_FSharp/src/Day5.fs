module Day5
open System.IO

let loadRules (filePath: string) : string list =
   File.ReadAllLines(filePath) |> List.ofArray 
  
let loadUpdates (filePath: string)  =
   File.ReadAllLines(filePath) |> List.ofArray |> List.map (fun x -> x.Split(",") |> List.ofArray)
 
let applyRules rules  updates =
   
   let rowOk (row, pairs) =
      //all pairs of rules are in rules
      pairs |> List.forall (fun p -> List.contains p rules )      

   let pairs r = 
    [ for i in 0 .. List.length r - 1 do
        for j in i + 1 .. List.length r - 1 do
            yield  $"{r.[i]}|{r.[j]}" ]
    
   let z = updates |> List.map (fun row -> (row, pairs row))
                   |> List.filter rowOk
                   |> List.map (fun (row, _) -> row)
                   |> List.map (fun r -> r.[(List.length r) / 2])
                   |> List.map (fun r -> int r)
                   |> List.sum

   printfn "Day5 Part1 %A" z
  
   
   
   
let part1() =
   let rules = loadRules "./Data/Day5Rules.txt"
   let updates = loadUpdates "./Data/Day5Updates.txt"
   applyRules rules updates 
   // printfn "Day5 Part1 rules %A" (loadRules "./Data/Day5Rules.txt")
   // printfn "Day5 Part1 updates  %A" (loadUpdates "./Data/Day5Updates.txt")
   //  
let part2() =
    printfn "Day5 Part2 "