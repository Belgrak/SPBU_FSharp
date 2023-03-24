module Interpreter

type Term =
    | Var of string
    | Lambda of string * Term
    | App of Term * Term

let rec substitute term varName replacement =
    match term with
    | Var v when v = varName -> replacement
    | Var v -> Var v
    | Lambda (v, e) when v = varName -> Lambda(v, e)
    | Lambda (v, e) when not (Set.contains v <| freeVars replacement) -> Lambda(v, substitute e varName replacement)
    | Lambda (v, e) ->
        let newVarName =
            freshVarName
            <| Set.union (freeVars e) (freeVars replacement)

        Lambda(newVarName, substitute (substitute e v (Var newVarName)) varName replacement)
    | App (e1, e2) -> App(substitute e1 varName replacement, substitute e2 varName replacement)

and freeVars term =
    match term with
    | Var v -> Set.singleton v
    | Lambda (v, e) -> Set.remove v <| freeVars e
    | App (e1, e2) -> Set.union (freeVars e1) (freeVars e2)

and freshVarName usedVars =
    let rec generate i =
        let varName = $"v%d{i}"

        if Set.contains varName usedVars then
            generate (i + 1)
        else
            varName

    generate 0

let rec reduce term =
    match term with
    | Var _ -> None
    | Lambda (v, e) -> reduce e |> Option.map (fun e' -> Lambda(v, e'))
    | App (Lambda (v, e1), e2) -> Some(substitute e1 v e2)
    | App (e1, e2) ->
        match reduce e1 with
        | Some e1' -> Some(App(e1', e2))
        | None -> reduce e2 |> Option.map (fun e2' -> App(e1, e2'))

let rec eval term =
    match reduce term with
    | Some reduced -> eval reduced
    | None -> term
