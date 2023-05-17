module Evaluator

open System

type EvaluatorBuilder(precision: int) =
    let round (x: float) = Math.Round(x, precision)

    member this.Bind(x, f) = f (round x)
    member this.Return(x) = round x
