module minesweeper

open System
open FSharpx

type Square = 
    |Bomb
    |Clear

    static member parse = function
                          |'*' -> Bomb
                          |_ -> Clear

    static member isBomb = (=) Bomb


type HintSquare =
    | Bomb
    | Hint of int

    static member getHint (square:Square) hintNumber = 
        match square with
        |Square.Bomb -> HintSquare.Bomb
        |Square.Clear -> HintSquare.Hint hintNumber

    static member render =
        function
        |Bomb -> '*'
        |Hint c -> char (int '0' + c) //ex. 6 -> '6'


let parse input =
    let words = Strings.toWords input
    let width = Int32.Parse (Seq.nth 1 words) //second word should be the width

    let parsedField = words 
                      |> Seq.skip 2
                      |> Seq.map (Seq.map (Square.parse))
    
    width, parsedField


let areNeighbours (x1,y1) (x2,y2)=
        let dx = x2 - x1 |> float
        let dy = y2 - y1 |> float
        let d = dx**2.0 + dy**2.0 |> sqrt
        d = 1.0 || d = sqrt 2.0


let calculateHints (field:seq<seq<Square>>) =
    let coordsField = 
        field
        |> Seq.mapi (fun y line ->
                        line |> Seq.mapi (fun x square -> (x,y),square))
        |> Seq.concat

    let countNeighbourBombs currentLocation =
        Seq.filter (fun (otherLocation, square) -> areNeighbours currentLocation otherLocation)
        >> Seq.filter (snd >> Square.isBomb)
        >> Seq.length
    
    let getHintSquare (location:int*int, square:Square) =
        HintSquare.getHint square (countNeighbourBombs location coordsField)

    coordsField
    |> Seq.map getHintSquare


let renderField width hintField =

    let rec getLines charField =
        seq {
             yield charField |> Seq.truncate width
             if Seq.length charField > width then
                yield! getLines (Seq.skip width charField)
        }
            
    hintField
    |> Seq.map HintSquare.render
    |> getLines
    |> Seq.map (String.Concat)
    |> String.concat "\r\n"

    
let createFieldWithHints input =
   let width, parsedField = parse input
   let hintField = calculateHints parsedField
   renderField width hintField