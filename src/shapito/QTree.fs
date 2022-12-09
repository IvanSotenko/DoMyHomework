module DoMyHomework.QTree

type QTree<'Value> =
    | Node of quadNW: QTree<'Value> * quadNE: QTree<'Value> * quadSW: QTree<'Value> * quadSE: QTree<'Value>
    | Leaf of value: 'Value
    | Empty


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

    let extract (x, y) =
        if x < rows && y < columns then
            let element = bas[x, y]
            match element with
            | Some a -> Leaf a
            | None -> Empty
        else Empty

    if size = 0 then Empty
    elif size = 1 then extract (0, 0)
    else

        let depth = int (System.Math.Ceiling (System.Math.Log(size, 2)))

        let rec constructSub level (x, y) =

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
