module Day3
open System.IO
open System.Text.RegularExpressions

type Ix = int
type X = int
type Y = int
type Instruction  =
    | Mul of Ix * X * Y
    | Do of Ix 
    | DoNot of Ix
    
let extractInstructions (str: string) =
    let regMul  =  Regex(@"mul\((\d+),(\d+)\)")
    let regDont = Regex(@"don't\(\)")
    let regDo = Regex(@"do\(\)")

 
    let muls =
        regMul.Matches(str)
            |> Seq.map (fun m -> 
                let matchIndex = m.Index
                let firstNumber = m.Groups.[1].Value
                let secondNumber = m.Groups.[2].Value
                Mul(matchIndex, int firstNumber, int secondNumber))
        
    let dos =
        regDo.Matches(str)
            |>  Seq.map (fun m -> 
                let matchIndex = m.Index 
                Do(matchIndex))
    let donts =
        regDont.Matches(str)
            |>  Seq.map (fun m -> 
                let matchIndex = m.Index 
                DoNot(matchIndex))
      // combine dos, donts and muls into one sequence sorted by Ix
    let all = Seq.concat [muls; dos; donts]
    let sorted = Seq.sortBy (fun x -> match x with | Mul(i, _, _) -> i | Do(i) -> i | DoNot(i) -> i) all
    let result = Seq.toList sorted
    result      

let evalInstructions (instr) =
    let mutable on = true
    let mutable acc = 0
    for i in instr do
        match i with
            | Mul(_, x, y) ->
                if on then acc  <- acc + x * y
            | Do(_) -> on <- true
            | DoNot(_) -> on <- false
    acc   
    
let part1 () =
        let lines = File.ReadAllLines("./Data/Day3.txt")
        String.concat "" lines |> extractInstructions |> evalInstructions |> printfn "Day3 Part1 %A"
        
       
    