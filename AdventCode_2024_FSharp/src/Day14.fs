module Day14
// p=0,4 v=3,-3
// p=6,3 v=-1,-3
// p=10,3 v=-1,2
// p=2,0 v=2,-1
// p=0,0 v=1,3
// p=3,0 v=-2,-2
// p=7,6 v=-1,-3
// p=3,0 v=-1,-2
// p=9,3 v=2,3
// p=7,3 v=-1,2
// p=2,4 v=2,-3
// p=9,5 v=-3,-3
open FParsec
let pInteger = pint32
let pWhitespace = spaces
let pLiteralString s = pstring s .>> pWhitespace
let pCoord prefix = pLiteralString prefix >>. pInteger

type X = int
type Y = int
type Pos = Pos of int * int
type Vel = Vel of int * int
type Robot = Robot of Pos * Vel

let pPos: Parser<Pos, unit> =
    pipe2 (pLiteralString "p=" >>. pInteger .>> pLiteralString ",") pInteger (fun x y -> Pos(x, y))

let pVel: Parser<Vel, unit> =
    pipe2 (pLiteralString " v=" >>. pInteger .>> pLiteralString ",") pInteger (fun x y -> Vel(x, y))

let pRobot: Parser<Robot, unit> = pipe2 pPos pVel (fun pos vel -> Robot(pos, vel))


let part1 () =
    let testInput = "p=7,3 v=-1,2"

    match run pRobot testInput with
    | Success(result, _, _) -> printfn "Parsed: %A" result
    | Failure(errorMsg, _, _) -> printfn "Error: %s" errorMsg
