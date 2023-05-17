module BracketSequence

let isBracketSequenceCorrect input =
    let rec loop (listInput, stack) =
        match listInput with
        | [] -> List.isEmpty stack
        | head::tail ->
            match head with
            | '(' | '[' | '{' -> loop (tail, head::stack)
            | ')' -> if stack.IsEmpty || stack.Head <> '(' then false else loop (tail, stack.Tail)
            | ']' -> if stack.IsEmpty || stack.Head <> '[' then false else loop (tail, stack.Tail)
            | '}' -> if stack.IsEmpty || stack.Head <> '{' then false else loop (tail, stack.Tail)
            | _ -> loop (tail, stack)
    loop (Seq.toList input, [])
