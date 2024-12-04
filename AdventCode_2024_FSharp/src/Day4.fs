module Day4

open System.IO
open System.Text.RegularExpressions


let charMatrix = array2D [
    [ 'M'; 'M'; 'M'; 'S'; 'X'; 'X'; 'M'; 'A'; 'S'; 'M' ]
    [ 'M'; 'S'; 'A'; 'M'; 'X'; 'M'; 'S'; 'M'; 'S'; 'A' ]
    [ 'A'; 'M'; 'X'; 'S'; 'X'; 'M'; 'A'; 'A'; 'M'; 'M' ]
    [ 'M'; 'S'; 'A'; 'M'; 'A'; 'S'; 'M'; 'S'; 'M'; 'X' ]
    [ 'X'; 'M'; 'A'; 'S'; 'A'; 'M'; 'X'; 'A'; 'M'; 'M' ]
    [ 'X'; 'X'; 'A'; 'M'; 'M'; 'X'; 'X'; 'A'; 'M'; 'A' ]
    [ 'S'; 'M'; 'S'; 'M'; 'S'; 'A'; 'S'; 'X'; 'S'; 'S' ]
    [ 'S'; 'A'; 'X'; 'A'; 'M'; 'A'; 'S'; 'A'; 'A'; 'A' ]
    [ 'M'; 'A'; 'M'; 'M'; 'M'; 'X'; 'M'; 'M'; 'M'; 'M' ]
    [ 'M'; 'X'; 'M'; 'X'; 'A'; 'X'; 'M'; 'A'; 'S'; 'X' ]
]
let getAllRows (matrix: 'a[,]) =
    [ for i in 0 .. Array2D.length1 matrix - 1 do
        [ for j in 0 .. Array2D.length2 matrix - 1 do
            yield matrix.[i, j] ] ]
    
let getAllColumns (matrix: 'a[,]) =
    [ for j in 0 .. Array2D.length2 matrix - 1 do
        [ for i in 0 .. Array2D.length1 matrix - 1 do
            yield matrix.[i, j] ] ]
    
    
let getAllDiagonals (matrix: 'a[,]) =
    let n = Array2D.length1 matrix // Assuming square matrix NxN
    let mainDiagonals =
        [ for d in -n+1 .. n-1 do
            [ for i in 0 .. n-1 do
                let j = i + d
                if j >= 0 && j < n then yield matrix.[i, j] ] ]

    let antiDiagonals =
        [ for d in 0 .. 2*(n-1) do
            [ for i in 0 .. n-1 do
                let j = d - i
                if j >= 0 && j < n then yield matrix.[i, j] ] ]
    
    (mainDiagonals, antiDiagonals)
    
    
let rowsColsDiags (matrix: 'a[,]) =
    // seq {
    //     yield getAllRows matrix
    //     yield getAllColumns matrix
    //     let mainDiags, antiDiags = getAllDiagonals matrix
    //     yield mainDiags
    //     yield antiDiags
    // }
    let rows = getAllRows matrix
    let cols = getAllColumns matrix
    let mainDiags, antiDiags = getAllDiagonals matrix
    rows, cols, mainDiags, antiDiags

let toStrings (m: char list list) : string list =
     List.map (fun row -> String.concat "" (List.map string row)) m
    // [ for l in m do
    //     yield String.concat "" (List.map string l) ]
    
let countXMAS (s: string) =
    let xmasRegex1 = Regex(@"XMAS")
    let xmasRegex2 = Regex(@"SAMX")
    
    xmasRegex1.Matches(s).Count + xmasRegex2.Matches(s).Count
let countMAS (s: string) =
    let masRegex1 = Regex(@"MAS")
    let masRegex2 = Regex(@"SAM")
    
    masRegex1.Matches(s).Count + masRegex2.Matches(s).Count

let inputData =
       let lines = File.ReadAllLines("/Users/mikehoughton/RiderProjects/AdventCode_2024_FSharp/AdventCode_2024_FSharp/Data/Day3.txt")
       let matrix = Array2D.init lines.Length lines.[0].Length (fun i j -> lines.[i].[j])
       matrix
       
let part1 () =
   
    //
    //
    let data = inputData 
    
    
    let r = getAllRows data
    let c = getAllColumns data
    let (m,  a) = getAllDiagonals data
    
    //let x = r |> List.map (fun row -> String.concat "" (List.map string row))
    let rc = r |>  toStrings |> List.map countXMAS |> List.sum 
    let cc = c |>  toStrings |> List.map countXMAS |> List.sum 
    let mc = m |>  toStrings |> List.map countXMAS |> List.sum
    let ac = a |>  toStrings |> List.map countXMAS |> List.sum 
    
    printfn "Part 1 D3 : %A" (rc + cc + mc + ac)
   
let fij (i: int) (j: int) (matrix: 'a[,]): bool =
    
//    checkGridForXmas(grid, y, x) {
//
//     return ((grid[y-1][x-1] === 'M' && grid[y+1][x+1] === 'S') || (grid[y-1][x-1] === 'S' && grid[y+1][x+1] === 'M'))
//         && ((grid[y+1][x-1] === 'M' && grid[y-1][x+1] === 'S') || (grid[y+1][x-1] === 'S' && grid[y-1][x+1] === 'M'))
//
// }
   ((matrix.[i - 1, j - 1] = 'M' && matrix.[i + 1, j + 1] = 'S') || (matrix.[i - 1, j - 1] = 'S' && matrix.[i + 1, j + 1] = 'M'))
   &&
   ((matrix.[i - 1, j + 1] = 'M' && matrix.[i + 1, j - 1] = 'S') || (matrix.[i - 1, j + 1] = 'S' && matrix.[i + 1, j - 1] = 'M'))
    
   
let part2 () =
    let data = inputData
    let mutable cnt = 0
    //loop over rows and columns    of data
    for i in 1 .. Array2D.length1 data - 2 do
        for j in 1 .. Array2D.length2 data - 2 do
            if data.[i, j] = 'A' then if fij i j data   then cnt <- cnt + 1 
              
            //printf "%c" data.[i, j]
    printfn "cnt %A" cnt
    
   
    // let (m,  a) = getAllDiagonals data
    //
    // //let x = r |> List.map (fun row -> String.concat "" (List.map string row))
    //
    // let mc = m |>  toStrings |> List.map countXMAS |> List.sum
    // let ac = a |>  toStrings |> List.map countXMAS |> List.sum
    //printfn "Part 2 D3 : %A %A %A" (mc + ac)  mc ac 
   
