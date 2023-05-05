module Rhombus

let drawRhombus n =
    let rec generate k =
        if abs k = n then
            String.init (n - 1) (fun i -> " ")
            + "*\n"
            + (if k < 0 then "" else generate (k - 1))
        else
            String.init ((abs k) - 1) (fun i -> " ")
            + "*"
            + String.init ((n - (abs k)) * 2 - 1) (fun i -> " ")
            + "*\n"
            + (if k = 1 then generate -2 else generate (k - 1))

    generate n
