module Tests

open NUnit.Framework
open Supermap
open Rhombus


let f () = printf $"{drawRhombus 4}"

[<Test>]
let ``first task example`` () =
    Assert.AreEqual([ sin 1.0; cos 1.0; sin 2.0; cos 2.0; sin 3.0; cos 3.0 ], supermap [ sin; cos ] [ 1.0; 2.0; 3.0 ])
