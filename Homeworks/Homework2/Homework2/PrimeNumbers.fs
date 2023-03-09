module PrimeNumbers

let getPrimeNumbers () =
    let rec isPrime n list =
        match list with
        | [] -> true
        | head :: tail when n % head <> 0 -> isPrime n tail
        | _ -> false

    let rec buildSequence n acc =
        seq {
            if isPrime n acc then
                yield n
                yield! buildSequence (n + 1) (n :: acc)
            else
                yield! buildSequence (n + 1) acc
        }

    buildSequence 2 []
