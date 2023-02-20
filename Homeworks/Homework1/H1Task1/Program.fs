let factorial x =
    let rec compute x acc =
        if x = 1 then acc else compute (x - 1) (acc * x)

    compute x 1

printfn $"%A{List.map factorial [ 1..10 ]}"
