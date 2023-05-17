module H4Task1Tests

open NUnit.Framework
open BracketSequence
open FsUnit

[<Test>]
let ``Empty string`` () =
    isBracketSequenceCorrect "" |> should equal true

[<Test>]
let ``One pair of balanced brackets`` () =
    isBracketSequenceCorrect "()" |> should equal true

[<Test>]
let ``One pair of unbalanced brackets`` () =
    isBracketSequenceCorrect "(]" |> should equal false

[<Test>]
let ``Multiple pairs of balanced brackets`` () =
    isBracketSequenceCorrect "([{}])" |> should equal true

[<Test>]
let ``Multiple pairs of unbalanced brackets`` () =
    isBracketSequenceCorrect "([})" |> should equal false

[<Test>]
let ``Multiple pairs of brackets with other characters`` () =
    isBracketSequenceCorrect "([{hello world}])" |> should equal true

[<Test>]
let ``Nested pairs of brackets`` () =
    isBracketSequenceCorrect "{ [ ( ) ] }" |> should equal true

[<Test>]
let ``Mismatched nested brackets`` () =
    isBracketSequenceCorrect "{ [ ( ] ) }" |> should equal false