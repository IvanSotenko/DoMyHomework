module DoMyHomework.BinTree

open System.Collections.Generic

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

                let right =
                    (subInit (level - 1) (i * 2 + 1))
                    |> binCollapse

                Node(left, right) |> binCollapse

        subInit depth 0


let ofVertList (verts: list<int * 'A>) (length: int) =
    let dict = new Dictionary<int, 'A>()

    for i in 0 .. verts.Length - 1 do
        dict.Add(verts[i])

    let initializer (index: int) =
        if dict.ContainsKey(index + 1) then
            Some(dict[index + 1])
        else
            None

    init length initializer


let ofArray (arr: Option<'A> []) = init arr.Length (fun i -> arr[i])


let ofUintList (verts: list<uint>) (length: int) (value: 'A) : BinTree<'A> =
    let set = Set.ofList verts

    let initializer (index: int) =
        if Set.contains (uint (index + 1)) set then
            Some value
        else
            None

    init length initializer
