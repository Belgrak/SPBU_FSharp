module Homework2Tests

open NUnit.Framework
open EvenCounter
open BinaryTree
open FsCheck

[<Test>]
let TestEvenCounter () =
    let checkEquality ls = List.forall ((=) (List.head ls)) ls

    let areSame testList =
        [ filterRealisation; foldRealisation; mapRealisation ]
        |> List.map (fun x -> x testList)
        |> checkEquality

    Check.QuickThrowOnFailure areSame


[<Test>]
let TestMapForTree () =
    let rec checkEquality first second func =
        match first, second with
        | Empty, Empty -> true
        | BinaryTree (firstVal, firstLeft, firstRight), BinaryTree (secondVal, secondLeft, secondRight) ->
            func firstVal = secondVal
            && checkEquality firstLeft secondLeft func
            && checkEquality firstRight secondRight func
        | _ -> false

    let areSame binaryTree testFunc =
        checkEquality binaryTree (mapForTree binaryTree testFunc) testFunc

    Check.QuickThrowOnFailure areSame
