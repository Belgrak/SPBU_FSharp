let fibonacci x =
    let rec compute n a b =
        if n >= x then a else compute (n + 1) b (a + b)

    compute 0 0 1

printfn $"%A{List.map fibonacci [ 1..10 ]}"
