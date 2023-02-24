module DoMyHomework.BinTree

open Microsoft.FSharp.Control

type BinTree<'Value> =
    | Node of leftChild: BinTree<'Value> * rightChild: BinTree<'Value>
    | Leaf of value: 'Value
    | Empty


let BinTreeToOption (tree: BinTree<'A>) : Option<'A> =
    match tree with
    | Leaf v -> Some v
    | Empty -> None
    | _ -> failwith "Unable to convert BinTree.Node to Option type"


let optionToBinTree (a: Option<'A>) : BinTree<'A> =
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


let rec foldBinTree (folder: 'State -> 'A -> 'State) (state: 'State) (tree: BinTree<'A>) =
    match tree with
    | Node (left, right) -> foldBinTree folder (foldBinTree folder state left) right
    | Leaf a -> folder state a
    | Empty -> state


/// Elementwise application of the function to the corresponding elements of both trees (with collapsing)
let addBinTree (tree1: BinTree<'A>) (tree2: BinTree<'B>) (func: Option<'A> -> Option<'B> -> Option<'C>) : BinTree<'C> =

    let rec addBinTreeSub tree1 tree2 =
        match tree1, tree2 with
        | Node (l1, r1), Node (l2, r2) ->
            Node(addBinTreeSub l1 l2, addBinTreeSub r1 r2)
            |> binCollapse
        | Node (l, r), leafOrEmpty ->
            Node(addBinTreeSub l leafOrEmpty, addBinTreeSub r leafOrEmpty)
            |> binCollapse
        | leafOrEmpty, Node (l, r) ->
            Node(addBinTreeSub leafOrEmpty l, addBinTreeSub leafOrEmpty r)
            |> binCollapse
        | leafOrEmpty1, leafOrEmpty2 ->
            (func (BinTreeToOption leafOrEmpty1) (BinTreeToOption leafOrEmpty2))
            |> optionToBinTree

    addBinTreeSub tree1 tree2


let init (count: int) (initializer: int -> Option<'A>) : BinTree<'A> =

    let extract ind =
        if ind < count then
            optionToBinTree (initializer ind)
        else
            Empty

    if count = 0 then
        Empty
    elif count = 1 then
        extract 0
    else

        let depth = int (System.Math.Ceiling(System.Math.Log(count, 2)))

        let rec subInit level i =

            if level = 1 then
                let left = extract (i * 2)
                let right = extract (i * 2 + 1)

                Node(left, right) |> binCollapse
            else
                let left = (subInit (level - 1) (i * 2)) |> binCollapse
                let right = (subInit (level - 1) (i * 2 + 1)) |> binCollapse

                Node(left, right) |> binCollapse

        subInit depth 0


let rec minInTree2 tree =
    match tree with
    | Empty -> failwith "Bro, it seems something wrong. Empty tree is not good"
    | Node (Empty, Empty) -> failwith "Bro, your tree didn't collapse, did it?"
    | Node (Empty, r) -> minInTree2 r
    | Node (l, Empty) -> minInTree2 l
    | Node (l, r) -> min (minInTree2 l) (minInTree2 r)
    | Leaf v -> v


let rec myMinInTree (tree: BinTree<'A>): Option<'A> =
    match tree with
    | Empty -> None
    | Leaf v -> Some v
    | Node (l, r) ->
        match (myMinInTree l), (myMinInTree r) with
        | None, None -> None
        | l, None -> l
        | None, r -> r
        | l, r -> min l r


let parallelMinInTree level tree =

    let rec collectTasks level tree =
        if level = 0 then
            [async {return myMinInTree tree}]
        else
            match tree with
            | Leaf _ | Empty -> [async {return myMinInTree tree}]
            | Node (l, r) -> (collectTasks (level - 1) l) @ (collectTasks (level - 1) r)

    let tasks = collectTasks level tree
    let values = tasks |> Async.Parallel |> Async.RunSynchronously

    Array.min values






// ############################################## EXAMPLE #################################################

// type BinTree2<'value> =
//     | FullNode of value: 'value * left: BinTree2<'value> * right: BinTree2<'value>
//     | NodeWithLeft of value: 'value * left: BinTree2<'value>
//     | NodeWithRight of value: 'value * right: BinTree2<'value>
//     | Leaf of value: 'value
//
// let rec minInTree tree =
//     match tree with
//     | Leaf v -> v
//     | NodeWithLeft (v, l) -> min v (minInTree l)
//     | NodeWithRight (v, r) -> min v (minInTree r)
//     | FullNode (v, l, r) -> min v (min (minInTree l) (minInTree r))
//
// let parallelMin2 level tree =
//     let valuesFromTopPart = ResizeArray()
//
//     let rec collectTasks level tree =
//         if level = 0 then
//             [ async { return minInTree tree } ]
//         else
//             match tree with
//             | Leaf _ -> [ async { return minInTree tree } ]
//             | NodeWithLeft (v, l) ->
//                 valuesFromTopPart.Add v
//                 collectTasks level l
//             | NodeWithRight (v, r) ->
//                 valuesFromTopPart.Add v
//                 collectTasks level r
//             | FullNode (v, l, r) ->
//                 valuesFromTopPart.Add v
//
//                 (collectTasks (level - 1) l)
//                 @ (collectTasks (level - 1) r)
//
//     let tasks = collectTasks level tree
//     let valuesFromBottomPart = tasks |> Async.Parallel |> Async.RunSynchronously
//
//     if valuesFromTopPart.Count > 0 then
//         min (Array.min valuesFromBottomPart) (valuesFromTopPart.ToArray() |> Array.min)
//     else
//         Array.min valuesFromBottomPart
