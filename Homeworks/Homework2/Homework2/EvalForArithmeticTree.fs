module ArithmeticTreeEval

type Operator =
    | Plus
    | Minus
    | Multiply
    | Divide

type ArithmeticTree<'a> =
    | ArithmeticTree of Operator * ArithmeticTree<'a> * ArithmeticTree<'a>
    | Leaf of 'a

let rec eval tree =
    match tree with
    | ArithmeticTree (operator, left, right) ->
        match operator with
        | Plus -> eval left + eval right
        | Minus -> eval left - eval right
        | Multiply -> eval left * eval right
        | Divide -> eval left / eval right
    | Leaf a -> a
