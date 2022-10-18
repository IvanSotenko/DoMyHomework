﻿module shapito.GeneralTree

open MyList

type GeneralTree<'value> =
    | Node of value: 'value * children: MyList<GeneralTree<'value>>
    | Leaf of value: 'value

let rec treeToSet tree =
        match tree with
            | Leaf v -> Set.empty.Add(v)
            | Node (v, children) ->
                    match children with
                    | Cons (kid, tail) -> (treeToSet kid) + Set.empty.Add(v) + (treeToSet (Node (v, tail)))
                    | Empty -> Set.empty


let countDistinct tree = Set.count (treeToSet tree)


let treeToList tree =

    let rec treeToListSub tree isis =
        match tree, isis with
        | Leaf v, _ -> Cons (v, Empty)

        | Node (v, children), false ->
            match children with
            | Cons (kid, tail) -> concat (treeToListSub kid true) (treeToListSub (Node (v, tail)) false)
            | Empty -> Empty

        | Node (v, children), true ->
            match children with
            | Cons (kid, tail) -> concat  (Cons (v, treeToListSub kid true)) (treeToListSub (Node (v, tail)) false)
            | Empty -> Empty

    treeToListSub tree true

// let testTree = Node ("a", Cons (Node ("b", Cons (Leaf "d", Cons (Leaf "e", Empty))), Cons (Leaf "c", Empty)))
