module DoMyHomework.BinTree

type BinTree<'value> =
    | FullNode of value: 'value * left: BinTree<'value> * right: BinTree<'value>
    | LeafWithLeft of value: 'value * left: BinTree<'value>
    | LeafWithRight of value: 'value * right: BinTree<'value>
    | Leaf of value: 'value

type BinTree2<'value> =
    | Node of value: 'value * left: Option<BinTree2<'value> * 'value> * right: Option<BinTree2<'value> * 'value>

let rec minInTree tree =
    match tree with
    | Leaf n -> n
    | FullNode (n, l, r) -> min n (min (minInTree l) (minInTree r))
    | LeafWithLeft (n, l) -> min n (minInTree l)
    | LeafWithRight (n, r) -> min n (minInTree r)
