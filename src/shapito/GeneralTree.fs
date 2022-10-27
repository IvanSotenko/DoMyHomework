module DoMyHomework.GeneralTree

open MyList

type GeneralTree<'value> = Node of value: 'value * children: MyList<GeneralTree<'value>>


// The problem was that function could not take both GeneralTree and MyList
// at the same time to move both "down" the tree and to the "right" on its list.
// The solution is to "wrap" the element in a Node each time so function works with the same type.
let rec toSet tree =
    match tree with
    | Node (v, children) ->
        match children with
        | Cons (kid, tail) ->
            (toSet kid)
            + Set.empty.Add(v)
            + (toSet (Node(v, tail)))
        | Empty -> Set.empty.Add(v)


let countDistinct tree = Set.count (toSet tree)


// The problem with the MyList was that when we "wrap" an element in a Node, it is duplicated.
// So by "wrapping" the element, we just tell the function that it does not need to be taken into account,
// passing the false parameter to it
let toMyList tree =

    let rec toListSub tree isis =
        match tree, isis with

        | Node (v, children), false ->
            match children with
            | Cons (kid, tail) -> concat (toListSub kid true) (toListSub (Node(v, tail)) false)
            | Empty -> Empty

        | Node (v, children), true ->
            match children with
            | Cons (kid, tail) -> concat (Cons(v, toListSub kid true)) (toListSub (Node(v, tail)) false)
            | Empty -> Cons(v, Empty)

    toListSub tree true

let collectInTree (joinFunc: 'a -> 'a -> 'a) (singleton: 'value -> 'a) (empty: 'a) (tree: GeneralTree<'value>) =

    let rec collectInTreeSub tree isNew =
        match tree, isNew with

        | Node (v, children), false ->
            match children with
            | Cons (kid, tail) -> joinFunc (collectInTreeSub kid true) (collectInTreeSub (Node(v, tail)) false)
            | Empty -> empty

        | Node (v, children), true ->
            match children with
            | Cons (kid, tail) -> joinFunc (joinFunc (singleton v) (collectInTreeSub kid true)) (collectInTreeSub (Node(v, tail)) false)
            | Empty -> singleton v

    collectInTreeSub tree true
