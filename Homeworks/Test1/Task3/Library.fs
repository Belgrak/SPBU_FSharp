module Option

type binOp =
    | Add
    | Sub
    | Mul
    | Div

type expr =
    | Var of string
    | Const of int
    | BinOp of binOp * expr * expr


type OptionBuilder() =
    member _.Bind(v, f) = Option.bind f v
    member _.Return v = Some v
    member _.Zero() = None

let opt = OptionBuilder()
