let listReverse list =
    let rec compute list acc =
        match list with
        | head :: tail -> compute tail (head :: acc)
        | [] -> acc

    compute list []

printf $"%A{listReverse [ 1..10 ]}"
