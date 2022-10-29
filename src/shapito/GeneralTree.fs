module DoMyHomework.GeneralTree

open MyList

type GeneralTree<'Value> = Node of value: 'Value * children: MyList<GeneralTree<'Value>>

let traversal (coreFunc: 'A -> 'A -> 'A) (singleton: 'Value -> 'A) (neutralElement: 'A) (tree: GeneralTree<'Value>) =

    let rec traversalSub tree isNew =
        match tree, isNew with

        | Node (v, children), false ->
            match children with
            | Cons (kid, tail) -> coreFunc (traversalSub kid true) (traversalSub (Node(v, tail)) false)
            | Empty -> neutralElement

        | Node (v, children), true ->
            match children with
            | Cons (kid, tail) ->
                coreFunc (coreFunc (singleton v) (traversalSub kid true)) (traversalSub (Node(v, tail)) false)
            | Empty -> singleton v

    traversalSub tree true


let toList tree =
    traversal concat (fun x -> Cons(x, Empty)) Empty tree

let toSet tree =
    traversal Set.union Set.empty.Add Set.empty tree

let countDistinct tree = tree |> toSet |> Set.count
