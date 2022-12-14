module DoMyHomework.BinTree

type BinTree<'Value> =
    | Node of leftChild: BinTree<'Value> * rightChild: BinTree<'Value>
    | Leaf of value: 'Value
    | Empty

let BinTreeToOption (tree: BinTree<'A>) : Option<'A> =
    match tree with
    | Leaf v -> Some v
    | Empty -> None
    | _ -> failwith "Unable to convert BinTree.Node to Option type"

let OptionToBinTree (a: Option<'A>) : BinTree<'A> =
    match a with
    | Some v -> Leaf v
    | None -> Empty


let binCollapse tree =
    match tree with
    | Node (Leaf a, Leaf b) when (a = b) -> Leaf a
    | Node (Empty, Empty) -> Empty
    | _ -> tree


let rec collapseBinTree tree =
    match tree with
    | Node (left, right) ->
        Node(collapseBinTree left, collapseBinTree right)
        |> binCollapse
    | _ -> tree


/// Reduce the depth of the tree by throwing out the right kids
let cutBinTree tree (reqLen: int) (actualLen: int) =

    if reqLen >= actualLen then
        tree
    else

        let depth = int (System.Math.Ceiling(System.Math.Log(actualLen, 2)))
        let targetDepth = int (System.Math.Ceiling(System.Math.Log(reqLen, 2)))

        let rec cut tree currentDepth =
            match tree with
            | Node (left, right) when currentDepth = targetDepth -> Node(left, right)
            | Node (left, _) -> cut left (currentDepth - 1)
            | Leaf a -> Leaf a
            | Empty -> Empty

        cut tree depth


/// Increases the depth of the tree by attaching Empty to the right
let expandBinTree tree (actualLen: int) (reqLen: int) =

    if reqLen <= actualLen then
        tree
    else

        let depth = int (System.Math.Ceiling(System.Math.Log(actualLen, 2)))
        let targetDepth = int (System.Math.Ceiling(System.Math.Log(reqLen, 2)))

        let rec expand tree curDepth =
            if curDepth < targetDepth then
                expand (Node(tree, Empty)) (curDepth + 1)
            else
                tree

        expand tree depth
