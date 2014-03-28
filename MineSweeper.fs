module minesweeper

open System
open FSharpx

type Square = Bomb|Clear of int


let splitLines input =
    let rec getLines str =
        seq {
            let idx = str |> Seq.tryFindIndex ((=) '\r')
            match idx with
            | Some n -> yield str |> Seq.take n
                        yield! (str |> Seq.skip (n+2) |> getLines)
            | None -> yield str
        }
    getLines input

                                 
let getCoordField lines =
    let getField =
        Seq.skip 1
        >> Seq.map (fun line ->
                        line
                        |> Seq.map (function
                                    |'*' -> Bomb
                                    |_ -> Clear 0)
                   )
                       
    let createCoords = 
        Seq.mapi (fun y line ->
                    line 
                    |> Seq.mapi (fun x v -> (x,y),v)
                 )
        >> Seq.concat

    lines
    |> getField
    |> createCoords


let getCountField field =
    let areNeighbours (x1,y1) (x2,y2)=
        let dx = x2 - x1 |> float
        let dy = y2 - y1 |> float
        let d = dx**2.0 + dy**2.0 |> sqrt
        d = 1.0 || d = sqrt 2.0

    let countNeighbourBombs cl =
        Seq.filter (fun (ol,s) -> areNeighbours cl ol)
        >> Seq.filter (fun (l,s) -> s = Bomb)
        >> Seq.length

    field
    |> Seq.map (fun (l,s) -> match s with
                             |Bomb -> l,Bomb
                             |Clear _ -> l,(Clear (countNeighbourBombs l field))
               )


let renderField field =
    let rowCount = 
        if Seq.isEmpty field 
        then 0
        else field
             |> Seq.map (fun ((x,y),s) -> y)
             |> Seq.max
               
    
    let lines =
        seq {
            for row in 0 .. rowCount do
            yield field
                  |> Seq.filter (fun ((x,y),s) -> y = row)
                  |> Seq.map (fun (l,s) -> match s with
                                           |Bomb -> '*'
                                           |Clear c -> char (int '0' + c) //ex. 6 -> '6'
                             )
                  |> String.Concat
        }
            
    String.concat "\r\n" lines

    
let createField input =
   input
   |> splitLines
   |> getCoordField
   |> getCountField
   |> renderField