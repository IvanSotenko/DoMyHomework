module shapito.GeneralTree

open MyList

type GeneralTree<'value> =
    | Node of value: 'value * children: MyList<GeneralTree<'value>>
    | Leaf of value: 'value

 let rec treeSet tree  =
     match tree with
     | Leaf v -> Set.empty.Add(v)
     | Node (v, children) ->
         match children with
         | Cons (kid, tail) -> Set.union (Set.union (treeSet kid) (Set.empty.Add(v))) (treeSet (Node (v, tail)))
         | Empty -> Set.empty

// let testTree = Node ("a", Cons (Node ("b", Cons (Leaf "d", Cons (Leaf "e", Empty))), Cons (Leaf "c", Empty)))
