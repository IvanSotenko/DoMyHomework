module DoMyHomework.BinTree

type BinTree<'Value> =
    | FullNode of value: 'Value * left: BinTree<'Value> * right: BinTree<'Value>
    | LeafWithLeft of value: 'Value * left: BinTree<'Value>
    | LeafWithRight of value: 'Value * right: BinTree<'Value>
    | Leaf of value: 'Value

type BinTree2<'Value> =
    | Node of value: 'Value * left: Option<BinTree2<'Value> * 'Value> * right: Option<BinTree2<'Value> * 'Value>

let rec minInTree tree =
    match tree with
    | Leaf n -> n
    | FullNode (n, l, r) -> min n (min (minInTree l) (minInTree r))
    | LeafWithLeft (n, l) -> min n (minInTree l)
    | LeafWithRight (n, r) -> min n (minInTree r)
