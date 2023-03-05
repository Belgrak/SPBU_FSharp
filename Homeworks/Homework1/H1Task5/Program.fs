let indexSearch list x =
    let rec compute list x acc =
        match list with
        | [] -> None
        | head :: _ when head = x -> Some acc
        | _ :: tail -> compute tail x (acc + 1)

    compute list x 0

printf $"%A{List.map (indexSearch [ 1..5 ]) [ 1..6 ]}"
