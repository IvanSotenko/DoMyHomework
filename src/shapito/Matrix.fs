module shapito.Matrix
open QTree

let rec collapseQTree tree =
    match tree with
    | Node (nw, ne, sw, se) ->
        let parts = collapseQTree nw, collapseQTree ne, collapseQTree sw, collapseQTree se
        match parts with
        | Leaf a, Leaf b, Leaf c, Leaf d when (a = b) && (b = c) && (c = d) -> Leaf a
        | Empty, Empty, Empty, Empty -> Empty
        | _ -> Node parts
    | _ -> tree

let constructQTree (bas: Option<'A> [,]) =

    let rows = Array2D.length1 bas
    let columns = Array2D.length2 bas
    let size = max rows columns
    let depth = int (System.Math.Ceiling (System.Math.Log(size, 2)))

    let rec constructSub level (x, y) =

        let extract (x, y) =
            if x < rows && y < columns then
                let element = bas[x, y]
                match element with
                | Some a -> Leaf a
                | None -> Empty
            else Empty

        if level = 1
        then
            let quadNW = extract (x*2, y*2)
            let quadNE = extract (x*2, y*2 + 1)
            let quadSW = extract (x*2 + 1, y*2)
            let quadSE = extract (x*2 + 1, y*2 + 1)
            Node(quadNW, quadNE, quadSW, quadSE)

        else
            Node((constructSub (level - 1) (x*2, y*2)),
                 (constructSub (level - 1) (x*2, y*2 + 1)),
                 (constructSub (level - 1) (x*2 + 1, y*2)),
                 (constructSub (level - 1) (x*2 + 1, y*2 + 1)))

    collapseQTree (constructSub depth (0, 0))

type Matrix<'A when 'A: equality> =
    val Data: QTree<'A>
    val Length1: int
    val Length2: int
    new(arr) = { Data = collapseQTree (constructQTree arr); Length1 = Array2D.length1 arr; Length2 = Array2D.length2 arr }
    new(tree, length1, length2) = { Data = tree; Length1 = length1; Length2 = length2 }

    member this.Item
        with get (row, column) =

            let size = int (2.**(System.Math.Ceiling (System.Math.Log(max this.Length1 this.Length2, 2))))

            if (row >= this.Length1) || (column >= this.Length2) then failwith "Matrix index out of range"
            else
                let rec find tree (y, x) =
                    match tree with
                    | Node (nw, ne, sw, se) ->
                        if row < y && column < x then
                            find nw (y/2, x/2)
                        elif row < y  && column >= x then
                            find ne (y/2, x)
                        elif row >= x && column < y then
                            find sw (y, x/2)
                        else
                            find se (y, x)

                    | Leaf a -> Some a
                    | Empty -> None

                find this.Data (size - 1, size - 1)
