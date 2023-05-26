module H3Task3Tests

open Interpreter
open NUnit.Framework
open FsUnit


[<Test>]
let combinatorKTest () =
    let term =
        App(App(Lambda("x", Lambda("y", Var("x"))), Var("z")), Var("w"))

    let expected = Var("z")
    let actual = eval term
    actual |> should equal expected

[<Test>]
let standardTest () =
    let value =
        eval (
            App(
                Lambda("x", Var "y"),
                App(Lambda("x", App(Var "x", App(Var "x", Var "x"))), Lambda("x", App(Var "x", App(Var "x", Var "x"))))
            )
        )

    value |> should equal (Var "y")

[<Test>]
let idTest () =
    let value =
        eval (App(Lambda("x", Var "x"), Var "z"))

    value |> should equal (Var "z")

[<Test>]
let lambdaIdTest () =
    let value =
        eval (App(Lambda("x", Var "x"), Lambda("x", Var "x")))

    value |> should equal (Lambda("x", Var "x"))


[<Test>]
let replacementTest () =
    let value =
        eval (App(Lambda("a", Lambda("x", Var "a")), Var "x"))

    value |> should equal (Lambda("v0", Var "x"))
