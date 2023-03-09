module BinaryTree

type BinaryTree<'a> =
    | BinaryTree of 'a * BinaryTree<'a> * BinaryTree<'a>
    | Empty

let rec mapForTree tree func =
    match tree with
    | Empty -> Empty
    | BinaryTree (a, left, right) -> BinaryTree(func a, mapForTree left func, mapForTree right func)
