module Day13

//https://numerics.mathdotnet.com/Euclid

open System.IO
open FParsec

type XY =
    { X: int64
      Y: int64 }

    override this.ToString() = $"XY(X = {this.X}, Y = {this.Y})\n"

type Button =
    { Name: string
      Movement: XY }

    override this.ToString() =
        $"Button (Name = {this.Name}, Movement = {this.Movement})\n"

//type Prize = { X: int; Y: int }
type GameData =
    { Buttons: Button list
      Prize: XY }

    override this.ToString() =
        $"GameData(Buttons!! = {this.Buttons}, Prize = {this.Prize})\n"


// Parsers for common components
let pInteger = pint64
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



let extendedEuclideanIterative (a: int64) (b: int64) (n: int64) =
    // Helper: Calculate GCD and coefficients using the iterative extended Euclidean algorithm
    let mutable x0, x1 = 1L, 0L
    let mutable y0, y1 = 0L, 1L
    let mutable r0, r1 = a, b

    while r1 <> 0 do
        let q = r0 / r1
        let r = r0 % r1
        r0 <- r1
        r1 <- r

        let x = x0 - q * x1
        x0 <- x1
        x1 <- x

        let y = y0 - q * y1
        y0 <- y1
        y1 <- y

    (r0, x0, y0) // GCD, x, y

let solveCramersRuleInteger (a1, b1, c1) (a2, b2, c2) : Option<int64 * int64> =
    // Calculate the determinant
    let delta = int64 a1 * b2 - a2 * b1

    if delta = 0 then
        None
    else


        // Calculate determinants for x and y
        let deltaX = c1 * b2 - c2 * b1
        let deltaY = a1 * c2 - a2 * c1

        // Check if the system allows integer solutions
        if deltaX % delta <> 0 || deltaY % delta <> 0 then
            None
        else

            // Solve for integer x and y
            let x = deltaX / delta
            let y = deltaY / delta

            Some(x, y)

// a_1x + b_1y = c_1
//
//
// a_2x + b_2y = c_2


let countAndGenerateSolutions a b n =
    // Step 1: Check if the equation is solvable
    let gcd, x0, y0 = extendedEuclideanIterative a b n

    if n % gcd <> 0 then
        printfn $"No solution exists for {a}x + {b}y = {n} because {n} is not divisible by GCD({a}, {b}) = {gcd}"
        None
    else

        // Step 2: Scale x and y to solve a * x + b * y = n
        let scale = n / gcd
        let x0 = x0 * scale
        let y0 = y0 * scale

        // Step 3: Define the range of solutions for non-negative integers
        let kStart = ceil (float -x0 * float gcd / float b) |> int
        let kEnd = floor (float y0 * float gcd / float a) |> int
        let stepX = (int64) b / gcd
        let stepY = (int64) -a / gcd

        let solutions =
            [ for k in kStart..kEnd -> (int64 (x0 + int64 k * stepX), int64 (y0 + int64 k * stepY)) ]

        Some(solutions.Length, solutions)

let processGameData (gameData: GameData list) =
    let mutable total = 0L

    for g in gameData do
        let bA = g.Buttons[0]
        let bB = g.Buttons[1]
        let p = g.Prize

        let ab =
            solveCramersRuleInteger (bA.Movement.X, bB.Movement.X, p.X + 10000000000000L) (bA.Movement.Y, bB.Movement.Y, int64 p.Y + 10000000000000L) 

        match ab with
        | Some(a, b) -> total <- total + (3L * int64 a + b)
        | _ -> ()


    total

let part1 () =
    let data = File.ReadAllText "./Data/Day13.txt"
    let gd = (parseInput data)
    let t = processGameData gd
    printfn ($"Day13 Part1 {t}")
