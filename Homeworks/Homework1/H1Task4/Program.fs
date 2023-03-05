let powersOfTwo n m =
    let rec compute x m =
        if m = 0 then
            [ x ]
        else
            x :: compute (x * 2) (m - 1)

    compute (pown 2 n) m

printf $"%A{powersOfTwo 1 9}"
