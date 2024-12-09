module Day9
type Entry = BlockFile | Space

let flipEntry (e: Entry) : Entry =
    match e with
    | BlockFile -> Space
    | Space -> BlockFile
    
//let expand (data: string) : string = ""

let rec expand (d: char list) : char list =
    
    
    let rec expand' (id: int) entry (data: char list) (result: char list) : char list =
      
           let g (i:int) (entry: Entry) (theChar: char) = 
               match entry with
               | Space -> List.replicate  id  ' '
               | BlockFile -> List.replicate  id  theChar
           
           match data with
            | [] -> result // Handle empty string
            | x :: xs -> expand' (id + 1) (flipEntry entry) xs (result @  g id entry x  )
                       
   // match entry with
   //                              | Space -> List.replicate  id  ' '
   //                              | BlockFile -> List.replicate  id  x
                               
               
                     

    expand' 0 BlockFile d []


let part1 () =
    let data = "12345" |> Seq.toList

    printfn "Day 9 Part 1 %A" (expand data)
    0
