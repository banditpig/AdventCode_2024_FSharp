module Day2

open System.IO

type ReportState =
    | Safe
    | Unsafe

type Slope =
    | Up
    | Down

let getSlope (a: int) (b: int) : Slope = if a < b then Up else Down

let checkReport (report: int list) : ReportState =
    // Helper function for recursion
    let rec check index slope =
        if index >= report.Length - 1 then
            Safe
        else
            let a, b = report.[index], report.[index + 1]

            match a, b with
            | _ when a = b -> Unsafe
            | _ when abs (a - b) > 3 -> Unsafe
            | _ when getSlope a b <> slope -> Unsafe
            | _ -> check (index + 1) slope
    // Initial call
    let slope = getSlope report.[0] report.[1]
    check 0 slope

let part1 () =

    let reports =
        File.ReadAllLines("./Data/Day2.txt")
        |> Array.map (fun line -> line.Split(' ') |> Array.map int |> Array.toList)
        |> Array.toList

    let x =
        reports
        |> List.map (fun r -> (r, checkReport r)) //checkReport
        |> List.filter (fun (_, x) -> x = Safe)
        |> List.length

    printfn "Day2 Part1 %A" x

let canFixReport2 (report: int list) : bool =
    // Remove one element at a time and check if the report is safe
    if report.Length <= 1 then
        false // Single-element reports can't be fixed
    else
        report
        |> List.mapi (fun idx _ ->
            let newReport =
                if idx = 0 then
                    report.[1..]
                else
                    report.[0 .. idx - 1] @ report.[idx + 1 ..]

            checkReport newReport)
        |> List.exists (fun state -> state = Safe)
// let canFixReport (report: int list) : bool =
//     //[1; 2; 7; 8; 9]
//     //remove one element at a time and check if the report is safe only allowed one fix
//     let check index =
//         let mutable idx = index
//         let mutable result = false
//
//         while idx < report.Length - 1 && not result do
//
//             let newReport = report.[0 .. idx - 1] @ report.[idx + 1 ..]
//
//
//
//             match checkReport newReport with
//             | Safe -> result <- true
//             | _ -> idx <- idx + 1
//
//
//         result
//
//     check 0
53 + 383 = 436

let part2 () =
    let reports =
        File.ReadAllLines("./Data/Day2.txt")
        |> Array.map (fun line -> line.Split(' ') |> Array.map int |> Array.toList)
        |> Array.toList

    let unsafeReports = reports |> List.filter (fun r -> checkReport r = Unsafe)

    let fixableReports = unsafeReports |> List.filter canFixReport2

    printfn "Day2 Part2 %A" (List.length fixableReports)
