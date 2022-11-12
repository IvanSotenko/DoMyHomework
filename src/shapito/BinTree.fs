module shapito.BinTree

type BinTree<'Value> =
    | Node of leftKid: BinTree<'Value> * rightKid: BinTree<'Value>
    | Leaf of value: 'Value
    | Empty
