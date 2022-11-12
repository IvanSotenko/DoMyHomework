module shapito.QTree

type QTree<'Value> =
    | Node of quadNW: QTree<'Value> * quadNE: QTree<'Value> * quadSW: QTree<'Value> * quadSE: QTree<'Value>
    | Leaf of value: 'Value
    | Empty


type Matrix(bas: int [,]) =
    let QuadTree, depth =
        let rows = Array2D.length1 bas
        let columns = Array2D.length2 bas
        let cube = max rows columns
        let depth = int (System.Math.Ceiling (System.Math.Log(cube, 2)))

        let rec loopy level (x, y): QTree<int> =
            if level = 1
            then
                let quadNW =
                    if x*2 < rows && y*2 < columns then Leaf(bas[x*2, y*2])
                    else Empty
                let quadNE =
                    if x*2 < rows && y*2 + 1 < columns then Leaf(bas[x*2, y*2 + 1])
                    else Empty
                let quadSW =
                    if x*2 + 1 < rows && y*2 < columns then Leaf(bas[x*2 + 1, y*2])
                    else Empty
                let quadSE =
                    if x*2 + 1 < rows && y*2 + 1 < columns then Leaf(bas[x*2 + 1, y*2 + 1])
                    else Empty

                Node(quadNW, quadNE, quadSW, quadSE)

            else
                Node((loopy (level - 1) (x*2, y*2)),
                     (loopy (level - 1) (x*2, y*2 + 1)),
                     (loopy (level - 1) (x*2 + 1, y*2)),
                     (loopy (level - 1) (x*2 + 1, y*2 + 1)))

        loopy depth (0, 0), depth

    member this.element (row, column) =
        let rec find level tree (curRow, curCol) =
            match tree with
            | Node (nw, ne, sw, se) ->
                match (curRow + int (2.**(float level)) > row), (curCol + int (2.**(float level)) > column) with
                | true, true -> find (level - 1) nw (curRow, curCol)
                | true, false -> find (level - 1) ne (curRow, curCol + int (2.**(float level)))
                | false, true -> find (level - 1) sw (curRow + int(2.**(float level)), curCol)
                | false, false -> find (level - 1) se (curRow + int (2.**(float level)), curCol + int (2.**(float level)))

            | Leaf v -> Some v
            | Empty -> None

        find (depth - 1) QuadTree (0, 0)
