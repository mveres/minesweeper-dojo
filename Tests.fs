module tests

open FsUnit
open NUnit.Framework
open minesweeper

[<Test>]
let ``Given 0 0 should output empty string`` () =
    "0 0" |> createFieldWithHints |> should equal ""

// 1 1   ->  *
// *
[<Test>]
let ``Given 1 1|* should output *`` () =
    ["1 1";
     "*"] 
    |> String.concat "\r\n"
    |> createFieldWithHints 
    |> should equal "*"

// 1 1   ->  0
// .
[<Test>]
let ``Given 1 1|, should output 0`` () =
    ["1 1";
     "."]
    |> String.concat "\r\n"
    |> createFieldWithHints
    |> should equal "0"

// 1 5   ->  1*2*1
// .*.*.
[<Test>]
let ``Given 1 5|,*,*, should output 1*2*1`` () =
    ["1 5";
     ".*.*."]
    |> String.concat "\r\n"
    |> createFieldWithHints 
    |> should equal "1*2*1"

// 2 2   ->  2*
// .*        2*
// .*
[<Test>]
let ``Given 2 2|,*|,* should output 2*|2*`` () =
    ["2 2";
     ".*";
     ".*"]
    |> String.concat "\r\n"
    |> createFieldWithHints
    |> should equal "2*\r\n2*"

// 1 10      ->  *1000000000
// *.........
[<Test>]
let ``Given 1 10|*,,,,,,,, should output *100000000`` () =
    ["1 10";
     "*........."]
    |> String.concat "\r\n"
    |> createFieldWithHints 
    |> should equal "*100000000"

// 3 3   ->  ***
// ***       *8*
// *.*       ***
// ***
[<Test>]
let ``Given 3 3|***|*,*|*** should output ***|*8*|***`` () =
    ["3 3";
     "***";
     "*.*";
     "***"]
    |> String.concat "\r\n"
    |> createFieldWithHints
    |> should equal "***\r\n*8*\r\n***"