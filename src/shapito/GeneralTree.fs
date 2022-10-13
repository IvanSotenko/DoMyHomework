module shapito.GeneralTree

open MyList

type GeneralTree<'value> =
    | Node of value: 'value * children: MyList<GeneralTree<'value>>
    | Leaf of value: 'value

// let tree = Node (4, Cons (Leaf 3, Cons (Leaf 6, Empty)))

// let treeSet tree list =
//
 // let rec map (kids: MyList<GeneralTree<'value>>) =
 //     match kids with
 //     | Cons (kid, tail) -> Set.union (move kid) (map tail)
 //     | Empty -> Set.empty

 // let move tree hashset =
 //     match tree with
 //     | Leaf v -> Set.empty.Add(v)
 //     | Node (v, children) -> Set.union (Set.add v hashset) (map children)

 // let rec move tree hashset =
 //     match tree with
 //     | Leaf v -> Set.empty.Add(v)
 //     | Node (v, children) ->
 //         match children with
 //         | Cons (kid, Empty) -> Set.union (move kid Set.empty) (Set.add v hashset)
 //         | Cons (kid, tail) -> Set.union (Set.union (move kid Set.empty) (Set.add v hashset)) (move (Node (v, tail)) hashset)
 //         | Empty -> Set.empty

 // let rec move2 tree  =
 //     match tree with
 //     | Leaf v -> Set.empty.Add(v)
 //     | Node (v, children) ->
 //         match children with
 //         | Cons (kid, Empty) -> Set.union (move2 kid) (Set.empty.Add(v))
 //         | Cons (kid, tail) -> Set.union (Set.union (move2 kid) (Set.empty.Add(v))) (move2 (Node (v, tail)))
 //         | Empty -> Set.empty

 let rec move tree hashset =
     match tree with
     | Leaf v -> Set.empty.Add(v)
     | Node (v, children) ->
         match children with
         | Cons (kid, tail) -> Set.union (Set.union (move kid Set.empty) (Set.add v hashset)) (move (Node (v, tail)) hashset)
         | Empty -> Set.empty

// let rec treeSet tree list =

// let rec treeSet (tree: Option<MyList<'value> * GeneralTree<'value>>) =
//     match tree with
//     | Cons -> 3
//     | Empty -> 4
//     | Leaf -> 4
//     | Node -> 5


// let rec set lst =
//     match lst with
//     | Empty -> Set.empty
//     | Cons (head, tail) -> Set.union (Set.empty.Add(head)) (set tail)

// let treeToList tree =
//     match tree with
//     | Leaf v -> v
//     | Node (v, kids) ->

// let test = Node ("a", Cons (Node ("b", Cons (Leaf "d", Cons (Leaf "e", Empty))), Cons (Leaf "c", Empty)))
