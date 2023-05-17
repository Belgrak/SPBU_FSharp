module StringEvaluator

open System

type StringEvaluatorBuilder() =
    let parse (str: string) =
        match Int32.TryParse str with
        | true, int -> Some int
        | _ -> None

    member this.Bind(x, f) =
        match parse x with
        | None -> None
        | Some b -> f b


    member this.Return(x) = Some x
