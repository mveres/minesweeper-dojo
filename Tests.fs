module tests

open FsUnit
open NUnit.Framework
open minesweeper

[<Test>]
let ``Given 0 0 should output empty string`` () =
    "0 0" |> createFields |> should equal ""

// 1 1   ->  *
// *
[<Test>]
let ``Given 1 1|* should output *`` () =
    "1 1\r\n*" |> createFields |> should equal "*"

// 1 1   ->  0
// .
[<Test>]
let ``Given 1 1|, should output 0`` () =
    "1 1\r\n." |> createFields |> should equal "0"


// 1 5   ->  1*2*1
// .*.*.
[<Test>]
let ``Given 1 5|,*,*, should output 1*2*1`` () =
    "1 5\r\n.*.*." |> createFields |> should equal "1*2*1"


// 2 2   ->  2*
// .*        2*
// .*
[<Test>]
let ``Given 2 2|,*|,* should output 2*|2*`` () =
    "2 2\r\n.*\r\n.*" |> createFields |> should equal "2*\r\n2*"


