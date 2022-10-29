﻿module DoMyHomework.GeneralTree

open MyList

type GeneralTree<'Value> = Node of value: 'Value * children: MyList<GeneralTree<'Value>>

let traversal (operation: 'A -> 'A -> 'A) (singleton: 'Value -> 'A) (neutralElement: 'A) (tree: GeneralTree<'Value>) =

    let separate tree =
        match tree with
        | Node (v, children) -> v, children

    let rec subTraversal list accumulator =
        match list with
        | Cons (tree, nextTrees) ->

            let v, children = separate tree
            let updatedAccumulator = operation accumulator (singleton v)

            operation (subTraversal children updatedAccumulator) (subTraversal nextTrees neutralElement)

        | Empty -> accumulator

    let v, children = separate tree
    subTraversal children (singleton v)


let toList tree =
    traversal concat (fun x -> Cons(x, Empty)) Empty tree

let toSet tree =
    traversal Set.union Set.empty.Add Set.empty tree

let countDistinct tree = tree |> toSet |> Set.count
