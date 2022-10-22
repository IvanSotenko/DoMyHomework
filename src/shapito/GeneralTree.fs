module DoMyHomework.GeneralTree

open MyList

type GeneralTree<'value> =
    | Node of value: 'value * children: MyList<GeneralTree<'value>>

let rec set tree =
        match tree with
            | Node (v, children) ->
                    match children with
                    | Cons (kid, tail) -> (set kid) + Set.empty.Add(v) + (set (Node (v, tail)))
                    | Empty -> Set.empty.Add(v)


let countDistinct tree = Set.count (set tree)

let toList tree =

    let rec toListSub tree isis =
        match tree, isis with

        | Node (v, children), false ->
            match children with
            | Cons (kid, tail) -> concat (toListSub kid true) (toListSub (Node (v, tail)) false)
            | Empty -> Empty

        | Node (v, children), true ->
            match children with
            | Cons (kid, tail) -> concat  (Cons (v, toListSub kid true)) (toListSub (Node (v, tail)) false)
            | Empty -> Cons (v, Empty)

    toListSub tree true
// let testTree = Node ("a", Cons (Node ("b", Cons (Leaf "d", Cons (Leaf "e", Empty))), Cons (Leaf "c", Empty)))
