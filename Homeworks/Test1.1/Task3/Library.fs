module Workflow

type OptionBuilder() =
    member _.Bind(v, f) = Option.bind f v
    member _.Return v = Some v
    member _.Zero() = None

let opt = OptionBuilder()

type movement =
    | Left of int
    | Right of int
    | Top of int
    | Bottom of int

let rec evalMoves (startPos: int * int) movesList =
    match movesList with
    | head :: tail ->
        opt {
            match head with
            | Left i ->
                if fst startPos >= i then
                    let! r = evalMoves (fst startPos - i, snd startPos) tail
                    return r
            | Right i ->
                let! r = evalMoves (fst startPos + i, snd startPos) tail
                return r
            | Top i ->
                let! r = evalMoves (fst startPos, snd startPos + i) tail
                return r
            | Bottom i ->
                if snd startPos >= i then
                    let! r = evalMoves (fst startPos, snd startPos - i) tail
                    return r
        }
    | [] -> opt { return startPos }
