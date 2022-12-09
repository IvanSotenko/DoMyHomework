module DoMyHomework.Matrix
open QTree

type Matrix<'A when 'A: equality> =
    val Data: QTree<'A>
    val Length1: int
    val Length2: int
    new(arr) = { Data = collapseQTree (constructQTree arr); Length1 = Array2D.length1 arr; Length2 = Array2D.length2 arr }
    new(tree, length1, length2) = { Data = tree; Length1 = length1; Length2 = length2 }

    member this.Item
        with get (x, y) =

            let size = int (2.**(System.Math.Ceiling (System.Math.Log(max this.Length1 this.Length2, 2))))

            if (x >= this.Length1) || (y >= this.Length2) then failwith "Matrix index out of range"
            else
                let rec find tree (curX, curY) barrier =

                    match tree with
                    | Node (nw, ne, sw, se) ->
                        if x <= (curX - barrier) && y <= (curY - barrier) then
                            find nw (curX - barrier, curY - barrier) (barrier/2)

                        elif x <= (curX - barrier) && y > (curY - barrier) then
                            find ne (curX - barrier, curY) (barrier/2)

                        elif x > (curX - barrier) && y <= (curY - barrier) then
                            find sw (curX, curY - barrier) (barrier/2)

                        else
                            find se (curX, curY) (barrier/2)

                    | Leaf a -> Some a
                    | Empty -> None

                find this.Data (size - 1, size - 1) (size/2)
