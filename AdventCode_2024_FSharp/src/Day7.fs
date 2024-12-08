module Day7

open System.IO

type Op =
    | Add
    | Mul

let generateExpressions numbers operators =
    let rec generateCombos (current: string) remaining =
        match remaining with
        | [] -> [ current.Split(";") ]
        | x :: xs ->
            operators
            |> List.collect (fun op -> generateCombos (current + ";" + op + ";" + string x) xs)

    match numbers with
    | []
    | [ _ ] -> [] // No combinations possible with less than two numbers
    | x :: xs -> generateCombos (string x) xs

let (||) (x: int64) (y: int64) : int64 = string x + string y |> int64


let evalExpression (expr: string[]) =
    let mutable result = int64 expr.[0] // Start with the first number
    let mutable i = 1 // Start from the first operator

    while i < expr.Length - 1 do
        let operator = expr.[i]
        let operand = int64 expr.[i + 1]

        match operator with
        | "+" -> result <- result + operand
        | "*" -> result <- result * operand
        | "||" -> result <- result || operand
        | _ -> failwithf "Unknown operator: %s" operator

        i <- i + 2 // Move to the next operator and operand

    result

let solveEqn ((target: int64), (numbers: int64 list)) operators : int64 list =
    //all possible combinations of numbers and operations
    let expressions = generateExpressions numbers operators

    expressions
    |> List.map (fun expr -> evalExpression expr)
    |> List.filter (fun x -> x = target)
    |> Set.ofList
    |> Set.toList


let getEquations (path: string) =

    File.ReadAllLines(path)
    |> Array.map (fun line ->
        let parts = line.Split(':') //both sides of the colon
        let target = int64 parts.[0] //left side of the colon as int
        //right side of the colon, split on a space, filter out spaces, convert to int, convert to list
        let numbers =
            parts.[1].Split(' ')
            |> Array.filter (fun s -> s <> "")
            |> Array.map int64
            |> Array.toList

        (target, numbers))


let part1 () =
    let eqns = getEquations "./Data/Day7.txt" |> Array.toList

    let result =
        List.fold (fun acc eqn -> acc + (solveEqn eqn [ "*"; "+" ] |> List.sum)) (0: int64) eqns

    printfn "Day7 Part1 %A" result

let part2 () =
    let eqns = getEquations "./Data/Day7.txt" |> Array.toList

    let result =
        List.fold (fun acc eqn -> acc + (solveEqn eqn [ "*"; "+"; "||" ] |> List.sum)) (0: int64) eqns

    printfn "Day7 Part2 %A" result
