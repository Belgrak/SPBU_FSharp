let indexSearch list x =
    let rec compute list x acc =
        match list with
        | head :: tail -> if head = x then acc else compute tail x (acc + 1)
        | [] -> -1

    compute list x 0

printf $"%A{List.map (indexSearch [ 1..5 ]) [ 1..6 ]}"
