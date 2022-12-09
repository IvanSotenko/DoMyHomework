module DoMyHomework.Vector

open BinTree

let constructBinTree (bas: array<Option<'A>>) =

    let length = Array.length bas

    let extract ind =
        if ind < length then
            match bas[ind] with
            | Some a -> Leaf a
            | None -> Empty
        else
            Empty

    if Array.isEmpty bas then
        Empty
    elif Array.length bas = 1 then
        extract 0
    else

        let depth = int (System.Math.Ceiling(System.Math.Log(length, 2)))

        let rec constructSub level i =

            if level = 1 then
                let left = extract (i * 2)
                let right = extract (i * 2 + 1)
                Node(left, right)
            else
                Node(constructSub (level - 1) (i * 2), constructSub (level - 1) (i * 2 + 1))

        constructSub depth 0

type Vector<'A when 'A: equality> =
    val Data: BinTree<'A>
    val Length: int

    new(arr) =
        { Data = collapseBinTree (constructBinTree arr)
          Length = arr.Length }

    new(tree, length) = { Data = tree; Length = length }

    member this.Item
        with get i =

            let len =
                int (
                    2.
                    ** (System.Math.Ceiling(System.Math.Log(this.Length, 2)))
                )

            if i >= this.Length then
                failwith "Vector index out of range"
            else

                let rec find tree i curI barrier =
                    match tree with
                    | Node (left, right) ->
                        if i > (curI - barrier) then
                            find right i curI (barrier / 2)
                        else
                            find left i (curI - barrier) (barrier / 2)
                    | Leaf a -> Some a
                    | Empty -> None

                find this.Data i (len - 1) (len / 2)
