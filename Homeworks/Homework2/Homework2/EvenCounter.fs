module EvenCounter

let isEven number = number % 2 = 0

let filterRealisation ls = List.filter isEven ls |> List.length

let foldRealisation list =
    List.fold (fun acc elem -> if isEven elem then acc + 1 else acc) 0 list

let mapRealisation ls =
    List.map (fun elem -> if isEven elem then 1 else 0) ls |> List.sum
