module shapito.GeneralTree

open MyList

type GeneralTree<'value> =
    | Node of value: Option<'value> * children: MyList<GeneralTree<'value>>
    | Leaf of value: 'value

 let rec treeSet tree =
     match tree with
     | Leaf v -> Set.empty.Add(v)
     | Node (Some v, children) ->
         match children with
         | Cons (kid, tail) -> (treeSet kid) + Set.empty.Add(v) + (treeSet (Node (Some v, tail)))
         | Empty -> Set.empty

 //  let rec treeList tree =
 //     match tree with
 //     | Leaf v -> Cons (v, Empty)
 //     | Node (Some v, children) ->
 //         match children with
 //         | Cons (kid, tail) -> concat (concat (Cons (v, Empty)) (treeList kid)) (treeList (Node (Some v, tail)))
 //         | Empty -> Empty

 let rec treeList2 tree =
     match tree with
     | Leaf v -> Cons (v, Empty)

     | Node (None, children) ->
         match children with
         | Cons (kid, tail) -> concat (treeList2 kid) (treeList2 (Node (None, tail)))
         | Empty -> Empty

     | Node (Some v, children) ->
         match children with
         | Cons (kid, tail) -> concat (concat (Cons (v, Empty)) (treeList2 kid)) (treeList2 (Node (None, tail)))
         | Empty -> Empty

// let testTree = Node ("a", Cons (Node ("b", Cons (Leaf "d", Cons (Leaf "e", Empty))), Cons (Leaf "c", Empty)))
