module shapito.Vector
open BinTree

type Vector(bas: int []) =
    let BinaryTree, depth =
        let length = Array.length bas
        let depth = int (System.Math.Ceiling (System.Math.Log(length, 2)))

        let rec construct level i: BinTree<int> =
            if level = 1
            then
                let left =
                    if i*2 < length then Leaf bas[i*2]
                    else Empty
                let right =
                    if i*2 + 1 < length then Leaf bas[i*2 + 1]
                    else Empty

                Node (left, right)
            else
                Node(construct (level - 1) (i*2),
                     construct (level - 1) (i*2 + 1))

        let rec collapse (tree: BinTree<int>) =
            match tree with
            | Node (Leaf a, Leaf b) when (a = b) -> Leaf a
            | Node (Empty, Empty) -> Empty

            | Node (left, right) ->
                let collapsed = Node (collapse left, collapse right)
                match collapsed with
                | Node (Leaf a, Leaf b) when (a = b) -> Leaf a
                | Node (Empty, Empty) -> Empty
                | _ -> collapsed

            | _ -> tree

        collapse (construct depth 0), depth

    member this.element i =
        let size = int (2.0**(float depth))
        if i >= size then failwith "Vector index out of range"
        else

        let rec find level tree curIndex =
            match tree with
            | Node (left, right) ->
                if curIndex + int (2.**(float level)) > i
                then
                    find (level - 1) left curIndex
                else
                    find (level - 1) right (curIndex + int (2.**(float level)))

            | Leaf v -> Some v
            | Empty -> None

        find (depth - 1) BinaryTree 0
