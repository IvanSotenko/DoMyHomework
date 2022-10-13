module shapito.GeneralTree

open MyList

type GeneralTree<'value> =
    | Node of value: 'value * children: MyList<GeneralTree<'value>>
    | Leaf of value: 'value

 let rec treeSet tree =
     match tree with
     | Leaf v -> Set.empty.Add(v)
     | Node (v, children) ->
         match children with
         | Cons (kid, tail) -> Set.union (Set.union (treeSet kid) (Set.empty.Add(v))) (treeSet (Node (v, tail)))
         | Empty -> Set.empty

  let rec treeList tree =
     match tree with
     | Leaf v -> Cons (v, Empty)
     | Node (v, children) ->
         match children with
         | Cons (kid, tail) -> concat (concat (Cons (v, Empty)) (treeList kid)) (treeList (Node (v, tail)))
         | Empty -> Empty

 let rec treeList2 tree =
     match tree with
     | Leaf v -> Cons (v, Empty)

     | Node (null, children) ->
         match children with
         | Cons (kid, tail) -> concat  (treeList kid) (treeList (Node (null, tail)))
         | Empty -> Empty

     | Node (v, children) ->
         match children with
         | Cons (kid, tail) -> concat (concat (Cons (v, Empty)) (treeList kid)) (treeList (Node (null, tail)))
         // | Cons (kid, tail) -> concat (concat (treeList kid) (Cons (v, Empty))) (treeList (Node (v, tail)))
         | Empty -> Empty

// let testTree = Node ("a", Cons (Node ("b", Cons (Leaf "d", Cons (Leaf "e", Empty))), Cons (Leaf "c", Empty)))
