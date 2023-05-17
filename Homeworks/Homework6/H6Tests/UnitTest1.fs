module H6Tests

open NUnit.Framework
open Evaluator
open StringEvaluator


[<Test>]
let ``Rounding with precision 3 returns expected result`` () =
    let rounding = EvaluatorBuilder

    let result =
        rounding 3 {
            let! a = 2.0 / 12.0
            let! b = 3.5
            return a / b
        }

    Assert.AreEqual(0.048, result)

[<Test>]
let ``Rounding with precision 2 returns expected result`` () =
    let rounding = EvaluatorBuilder

    let result =
        rounding 2 {
            let! x = 2.0 / 12.0
            let! y = 3.5
            return x / y
        }

    Assert.AreEqual(0.05, result)

[<Test>]
let ``Binding valid string returns Some result`` () =
    let calculate = StringEvaluatorBuilder()

    let result =
        calculate {
            let! x = "1"
            let! y = "2"
            let z = x + y
            return z
        }


    Assert.AreEqual(Some 3, result)

[<Test>]
let ``Binding invalid string returns None`` () =
    let calculate = StringEvaluatorBuilder()

    let result =
        calculate {
            let! x = "1"
            let! y = "Ъ"
            let z = x + y
            return z
        }


    Assert.AreEqual(None, result)
