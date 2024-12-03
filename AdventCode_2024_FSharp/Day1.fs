module AdventCode_2024_FSharp.day1

open System.IO

let readStringsFromFile (filePath: string) : string list =
    try
        File.ReadAllLines(filePath) |> List.ofArray
    with
    | :? FileNotFoundException as ex ->
        printfn $"File not found: %s{ex.Message}"
        []
    | :? IOException as ex ->
        printfn $"Error reading file: %s{ex.Message}"
        []
       
   
     
let sumDiffs xs ys : int =
    List.map2 (fun x y -> abs (x - y)) xs ys |> List.sum
   
let updateCount (key: 'a) (dict: Map<'a, int>) =
    Map.change key (function
        | Some count -> Some (count + 1)
        | None -> Some 1
    ) dict
    
let createLists =
     readStringsFromFile "./Data/Day1.txt"
                |> List.map (fun line -> line.Split("   "))
                |> List.map (fun line -> (line.[0], line.[1]))
                |> List.map (fun (a,b)    -> (int a, int b))
 

let part1 =
     let lists = createLists
     let (left, right) = List.unzip lists |> fun (l, r) -> (List.sort l, List.sort r)
     printfn "%A" (sumDiffs left right)
     
let part2 =
    let lists = createLists
                
    let (left, right) = List.unzip lists |> fun (l, r) -> (List.sort l, List.sort r)
    let table = right |> List.fold  (fun acc  x -> updateCount x acc) Map.empty<int, int>
    
    let xx = left |> List.map (fun   x -> x *  (Map.tryFind x table |> Option.defaultValue 0))
             |> List.sum
    
   
    printfn "%A" xx
    

               
  
    