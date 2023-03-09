module Homework2Tests

open NUnit.Framework
open EvenCounter
open BinaryTree
open ArithmeticTreeEval
open PrimeNumbers
open FsCheck
open FsUnit

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

    let testMap binaryTree testFunc =
        checkEquality binaryTree (mapForTree binaryTree testFunc) testFunc

    Check.QuickThrowOnFailure testMap


[<Test>]
let TestArithmeticTreeEval () =
    let plusTest a b =
        eval (ArithmeticTree(Plus, Leaf a, Leaf b)) = a + b

    let minusTest a b =
        eval (ArithmeticTree(Minus, Leaf a, Leaf b)) = a - b

    let multiplyTest a b =
        eval (ArithmeticTree(Multiply, Leaf a, Leaf b)) = a * b

    let complexSumTest a b c =
        eval (ArithmeticTree(Plus, ArithmeticTree(Multiply, Leaf a, Leaf b), Leaf c)) = a * b + c

    let complexMultiplyTest a b c =
        eval (ArithmeticTree(Multiply, Leaf c, ArithmeticTree(Plus, Leaf a, Leaf b))) = c * (a + b)

    Check.QuickThrowOnFailure plusTest
    Check.QuickThrowOnFailure minusTest
    Check.QuickThrowOnFailure multiplyTest
    Check.QuickThrowOnFailure complexSumTest
    Check.QuickThrowOnFailure complexMultiplyTest

[<Test>]
let TestPrimeNumbers () =
    let getPrimeNumsSlice = [ for i in 0..9 -> Seq.item i (getPrimeNumbers ()) ]

    getPrimeNumsSlice |> should equal [ 2; 3; 5; 7; 11; 13; 17; 19; 23; 29 ]
