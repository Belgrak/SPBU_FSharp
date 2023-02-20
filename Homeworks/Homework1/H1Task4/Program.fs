let powersOfTwo n m =
    let rec compute x m acc =
        if m = 0 then
            x :: acc
        else
            x :: (compute (x * 2) (m - 1) acc)

    compute (pown 2 n) m []

printf $"%A{powersOfTwo 1 9}"
