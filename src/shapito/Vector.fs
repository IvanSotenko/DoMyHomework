module shapito.Vector
open BinTree

let constructBinTree (bas: array<Option<'A>>) =

    let length = Array.length bas
    let depth = int (System.Math.Ceiling (System.Math.Log(length, 2)))

    let rec constructSub level i =

        let extract ind =
            if ind < length then
                match bas[ind] with
                | Some a -> Leaf a
                | None -> Empty
            else Empty

        if level = 1
        then
            let left = extract (i*2)
            let right = extract (i*2 + 1)
            Node (left, right)
        else
            Node(constructSub (level - 1) (i*2),
                 constructSub (level - 1) (i*2 + 1))

    constructSub depth 0

let rec collapseBinTree tree =
    match tree with
    | Node (left, right) ->
        let collapsed = Node (collapseBinTree left, collapseBinTree right)
        match collapsed with
        | Node (Leaf a, Leaf b) when (a = b) -> Leaf a
        | Node (Empty, Empty) -> Empty
        | _ -> collapsed

    | _ -> tree

type Vector<'A when 'A: equality> =
    val Data: BinTree<'A>
    val Length: int
    new(arr) = { Data = collapseBinTree (constructBinTree arr); Length = arr.Length }
    new(tree, length) = { Data = tree; Length = length }

    member this.Item
        with get i =
            if i >= this.Length then failwith "Vector index out of range"
            else

                let rec find tree i curIndex =
                    match tree with
                    | Node (right, left) ->
                        if i < curIndex then
                            find right i (curIndex / 2)
                        else
                            find left i curIndex
                    | Leaf a -> Some a
                    | Empty -> None

                find this.Data i this.Length

