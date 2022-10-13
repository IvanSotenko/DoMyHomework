module shapito.Tree

type BinTree<'value> =
    | FullNode of value: 'value * left: BinTree<'value> * right: BinTree<'value>
    | LeafWithLeft of value: 'value * left: BinTree<'value>
    | LeafWithRight of value: 'value * right: BinTree<'value>
    | Leaf of value: 'value
    // | Empty

type BinTree2<'value> =
    | Node of value: 'value * left: Option<BinTree2<'value> * 'value> * right: Option<BinTree2<'value> * 'value>

let rec minInTree tree =
    match tree with
    | Leaf n -> n
    | FullNode (n, l, r) -> min n (min (minInTree l) (minInTree r))
    | LeafWithLeft (n, l) -> min n (minInTree l)
    | LeafWithRight (n, r) -> min n (minInTree r)

let tree = FullNode(5, FullNode(3, FullNode (8, Leaf 2, Leaf 9), Leaf 6), LeafWithLeft(4, FullNode (10, Leaf 11, Leaf 7)))
