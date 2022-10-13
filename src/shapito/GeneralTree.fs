module shapito.GeneralTree

open MyList

type GeneralTree<'value> =
    | Node of value: 'value * children: MyList<GeneralTree<'value>>
    | Leaf of value: 'value

// let tree = Node (4, Cons (Leaf 3, Cons (Leaf 6, Empty)))
