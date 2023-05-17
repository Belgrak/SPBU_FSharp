module WorkflowTests

open NUnit.Framework
open Workflow


[<Test>]
let ``no moves test`` () =
    Assert.AreEqual(Some(0, 0), evalMoves (0, 0) [])

[<Test>]
let ``move to negative cords test`` () =
    Assert.AreEqual(None, evalMoves (0, 0) [ Left 5 ])

[<Test>]
let ``some right moves test`` () =
    Assert.AreEqual(Some(5, 10), evalMoves (0, 0) [ Right 5; Top 10 ])

[<Test>]
let ``right and wrong moves test`` () =
    Assert.AreEqual(None, evalMoves (0, 0) [ Right 5; Bottom 10 ])

[<Test>]
let ``boundary tests`` () =
    Assert.AreEqual(Some(0, 0), evalMoves (3, 9) [ Left 3; Bottom 9 ])
