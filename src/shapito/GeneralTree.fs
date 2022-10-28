module DoMyHomework.GeneralTree

open MyList

type GeneralTree<'value> = Node of value: 'value * children: MyList<GeneralTree<'value>>

let collectInTree (joinFunc: 'a -> 'a -> 'a) (singleton: 'value -> 'a) (empty: 'a) (tree: GeneralTree<'value>) =

    let rec collectInTreeSub tree isNew =
        match tree, isNew with

        | Node (v, children), false ->
            match children with
            | Cons (kid, tail) -> joinFunc (collectInTreeSub kid true) (collectInTreeSub (Node(v, tail)) false)
            | Empty -> empty

        | Node (v, children), true ->
            match children with
            | Cons (kid, tail) ->
                joinFunc (joinFunc (singleton v) (collectInTreeSub kid true)) (collectInTreeSub (Node(v, tail)) false)
            | Empty -> singleton v

    collectInTreeSub tree true


let toList tree =
    collectInTree concat (fun x -> Cons(x, Empty)) Empty tree

let toSet tree =
    collectInTree Set.union Set.empty.Add Set.empty tree

let countDistinct tree = Set.count (toSet tree)
