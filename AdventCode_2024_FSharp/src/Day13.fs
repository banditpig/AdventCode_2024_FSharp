module Day13

//https://numerics.mathdotnet.com/Euclid

open System.IO
open FParsec

type XY = { X: int; Y: int }
type Button = { Name: string; Movement: XY }
//type Prize = { X: int; Y: int }
type GameData = { Buttons: Button list; Prize: XY }

// Parsers for common components
let pInteger = pint32
let pWhitespace = spaces
let pLiteralString s = pstring s .>> pWhitespace
let pCoord prefix = pLiteralString prefix >>. pInteger

// Parser for a single movement (e.g., "X+94, Y+34")
let pMovement =
    pipe2
        (pCoord "X+" <|> pCoord "X-") // Parse X movement
        (pLiteralString "," >>. pCoord "Y+" <|> pCoord "Y-") // Parse Y movement
        (fun x y -> { X = x; Y = y })

// Parser for a button (e.g., "Button A: X+94, Y+34")
let pButton =
    pipe2 (pLiteralString "Button" >>. anyChar .>> pLiteralString ":") pMovement (fun  // Parse the movement
                                                                                      name
                                                                                      movement ->
        { Name = string name
          Movement = movement })

// Parser for the prize location (e.g., "Prize: X=8400, Y=5400")
let pPrize =
    pipe2
        (pLiteralString "Prize:" >>. pCoord "X=") // Parse X coordinate
        (pLiteralString "," >>. pCoord "Y=") // Parse Y coordinate
        (fun x y -> { X = x; Y = y })

// Parser for the entire input
let pGameData =

    pipe2 (many1 (pButton .>> pWhitespace)) pPrize (fun  // Parse the prize
                                                        buttons
                                                        prize -> { Buttons = buttons; Prize = prize })

// Parser for multiple GameData sets
let pMultipleGameData = sepBy pGameData (newline >>. newline)

// Function to parse the input string
let parseInput input =
    match run pMultipleGameData input with
    | Success(result, _, _) -> result
    | Failure(errorMsg, _, _) -> failwith errorMsg

let part1 () =
    //     let data =


    let data = File.ReadAllText "./Data/Day13.txt"
    printfn "Day13 Part1 %A" (parseInput data)
