module Supermap

let supermap functions arguments =
    List.concat (List.map (fun x -> (List.map (fun f -> f x) functions)) arguments)
