module minesweeper

open System

let split input =
   let rec getLine str =
      seq {
         let idx = str |> Seq.tryFindIndex ((=) '\r')
         match idx with
         | Some n -> yield str |> Seq.take n
                     yield! (str |> Seq.skip (n+2) |> getLine)

         | None -> yield str
      }
   getLine input

let getSize sizeSeq =
   let intAt i= sizeSeq |> Seq.nth i |> fun c -> Int32.Parse(c.ToString())
   (intAt 0),(intAt 2)

let createCoordField height lines =
   let field = seq {
                     for y = 0 to (height-1) do
                     yield (lines |> Seq.nth y |> Seq.mapi (fun x c -> x,y,c))
                   }

   field |> Seq.concat

let isNeighbour (x1,y1) (x2,y2) =
   let dx = abs(x2-x1)
   let dy = abs(y2-y1)
   (dx + dy) >= 1 && (dx + dy) <= 2 && dx < 2 && dy < 2

let getNeighbours (x,y) field =
   field |> Seq.filter (fun (xi,yi,v) -> isNeighbour (xi,yi) (x,y))

let getSquareValue field (x,y,v) =
   if v = '*'
   then id v
   else
      field
      |> getNeighbours (x,y)
      |> Seq.filter (fun (x,y,v) -> v = '*') |> Seq.length
      |> fun count -> char (count.ToString())

let insertNewLines width field =
      let appendNewLine str = Seq.append (Seq.append str ['\r'] ) ['\n']
      let rec getLines str =
         seq {
            let length = str |> Seq.length
            if length > width
            then
               yield! (str |> Seq.take width |> appendNewLine)
               yield! getLines (str |> Seq.skip width)
            else
               yield! str
         }
      getLines field

let createFields input =
   let splitInput = input |> split
   let height,width =
      splitInput
      |> Seq.nth 0 |> getSize
   let coordField = splitInput |> Seq.skip 1 |> createCoordField height
   coordField |> Seq.map (getSquareValue coordField) |> insertNewLines width