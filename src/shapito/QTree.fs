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
