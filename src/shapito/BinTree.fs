module DoMyHomework.BinTree

type BinTree<'Value> =
    | Node of leftKid: BinTree<'Value> * rightKid: BinTree<'Value>
    | Leaf of value: 'Value
    | Empty


let addBinTree (tree1: BinTree<'A>) (tree2: BinTree<'A>) (addFunc: 'A -> 'A -> 'A): BinTree<'A> =

    let rec treePlusSub tree1 tree2 =
        match tree1, tree2 with
        | Node (l1, r1), Node(l2, r2) -> Node(treePlusSub l1 l2, treePlusSub r1 r2)
        | Node (l, r), leafOrEmpty -> Node(treePlusSub l leafOrEmpty, treePlusSub r leafOrEmpty)
        | leafOrEmpty, Node (l, r) -> Node(treePlusSub l leafOrEmpty, treePlusSub r leafOrEmpty)
        | Leaf a, Leaf b -> Leaf (addFunc a b)
        | leafOrEmpty, Empty -> leafOrEmpty
        | Empty, leafOrEmpty -> leafOrEmpty

    treePlusSub tree1 tree2


let rec collapseBinTree tree =
    match tree with
    | Node (left, right) ->
        let parts = collapseBinTree left, collapseBinTree right
        match parts with
        | Leaf a, Leaf b when (a = b) -> Leaf a
        | Empty, Empty -> Empty
        | _ -> Node(parts)

    | _ -> tree


let cutBinTree tree (reqLen: int) (actualLen: int) =

    if reqLen >= actualLen then tree
    else

        let depth = int (System.Math.Ceiling (System.Math.Log(actualLen, 2)))
        let targetDepth = int (System.Math.Ceiling (System.Math.Log(reqLen, 2)))

        let rec cut tree currentDepth =
            match tree with
            | Node (left, right) when currentDepth = targetDepth -> Node (left, right)
            | Node (left, _) -> cut left (currentDepth - 1)
            | Leaf a -> Leaf a
            | Empty -> Empty

        cut tree depth


let expandBinTree tree (actualLen: int) (reqLen: int) =

    if reqLen <= actualLen then tree
    else

        let depth = int (System.Math.Ceiling (System.Math.Log(actualLen, 2)))
        let targetDepth = int (System.Math.Ceiling (System.Math.Log(reqLen, 2)))

        let rec expand tree curDepth =
            if curDepth < targetDepth then expand (Node(tree, Empty)) (curDepth + 1)
            else tree

        expand tree depth
