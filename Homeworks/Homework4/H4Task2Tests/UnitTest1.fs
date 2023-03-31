module H4Task2Tests

open NUnit.Framework
open FsCheck
open PointFree

[<Test>]
let TestCorrectness () =
    let checkEquality ls = List.forall ((=) (List.head ls)) ls

    let areSame testNum testList =
        [ func; func1; func2; func3 ]
        |> List.map (fun x -> x testNum testList)
        |> checkEquality
    
    areSame
