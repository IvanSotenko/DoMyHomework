module shapito.Matrix
open QTree

type Matrix<'A when 'A: equality>(bas: 'A [,] when 'A: equality) =

    let rows = Array2D.length1 bas
    let columns = Array2D.length2 bas
    let size = max rows columns
    let depth = int (System.Math.Ceiling (System.Math.Log(size, 2)))

    member this.actualLen1 = Array2D.length1 bas
    member this.actualLen2 = Array2D.length2 bas

    member this.QuadTree =
        let rec construct level (x, y) =
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
                Node((construct (level - 1) (x*2, y*2)),
                     (construct (level - 1) (x*2, y*2 + 1)),
                     (construct (level - 1) (x*2 + 1, y*2)),
                     (construct (level - 1) (x*2 + 1, y*2 + 1)))

        let rec collapse tree =
            match tree with
            | Node (nw, ne, sw, se) ->
                let collapsed = Node (collapse nw, collapse ne, collapse sw, collapse se)
                match collapsed with
                | Node (Leaf a, Leaf b, Leaf c, Leaf d) when (a = b) && (b = c) && (c = d) -> Leaf a
                | Node (Empty, Empty, Empty, Empty) -> Empty
                | _ -> collapsed
            | _ -> tree

        collapse (construct depth (0, 0))

    member this.getItem (row, column) =
        if (row >= size) || (column >= size) then failwith "Matrix index out of range"

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

        find (depth - 1) this.QuadTree (0, 0)
