module DoMyHomework.QTree

type QTree<'Value> =
    | Node of quadNW: QTree<'Value> * quadNE: QTree<'Value> * quadSW: QTree<'Value> * quadSE: QTree<'Value>
    | Leaf of value: 'Value
    | Empty

let QTreeToOption (tree: QTree<'A>): Option<'A> =
    match tree with
    | Leaf v -> Some v
    | Empty -> None
    | _ -> failwith "Unable to convert QTree.Node to Option type"

let OptionToQTree (a: Option<'A>): QTree<'A> =
    match a with
    | Some v -> Leaf v
    | None -> Empty

let rec collapseQTree tree =
    match tree with
    | Node (nw, ne, sw, se) ->
        let parts = collapseQTree nw, collapseQTree ne, collapseQTree sw, collapseQTree se

        match parts with
        | Leaf a, Leaf b, Leaf c, Leaf d when (a = b) && (b = c) && (c = d) -> Leaf a
        | Empty, Empty, Empty, Empty -> Empty
        | _ -> Node parts
    | _ -> tree
