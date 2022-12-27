module DoMyHomework.QTree

open System.Collections.Generic

type QTree<'Value> =
    | Node of quadNW: QTree<'Value> * quadNE: QTree<'Value> * quadSW: QTree<'Value> * quadSE: QTree<'Value>
    | Leaf of value: 'Value
    | Empty


let QTreeToOption (tree: QTree<'A>) : Option<'A> =
    match tree with
    | Leaf v -> Some v
    | Empty -> None
    | _ -> failwith "Unable to convert QTree.Node to Option type"


let OptionToQTree (a: Option<'A>) : QTree<'A> =
    match a with
    | Some v -> Leaf v
    | None -> Empty


let qCollapse tree =
    match tree with
    | Node (Leaf a, Leaf b, Leaf c, Leaf d) when (a = b) && (b = c) && (c = d) -> Leaf a
    | Node (Empty, Empty, Empty, Empty) -> Empty
    | _ -> tree


let rec collapseQTree tree =
    match tree with
    | Node (nw, ne, sw, se) ->
        Node(collapseQTree nw, collapseQTree ne, collapseQTree sw, collapseQTree se)
        |> qCollapse
    | _ -> tree


let init length1 length2 (initializer: int -> int -> Option<'A>) : QTree<'A> =

    let extract (x, y) =
        if x < length1 && y < length2 then
            OptionToQTree(initializer x y)
        else
            Empty

    let size = max length1 length2

    if size = 0 then
        Empty
    elif size = 1 then
        extract (0, 0)
    else

        let depth = int (System.Math.Ceiling(System.Math.Log(size, 2)))

        let rec subInit level (x, y) =

            if level = 1 then
                Node(
                    extract (x * 2, y * 2),
                    extract (x * 2, y * 2 + 1),
                    extract (x * 2 + 1, y * 2),
                    extract (x * 2 + 1, y * 2 + 1)
                )
                |> qCollapse

            else
                Node(
                    (subInit (level - 1) (x * 2, y * 2)),
                    (subInit (level - 1) (x * 2, y * 2 + 1)),
                    (subInit (level - 1) (x * 2 + 1, y * 2)),
                    (subInit (level - 1) (x * 2 + 1, y * 2 + 1))
                )
                |> qCollapse

        subInit depth (0, 0)


let ofArray2D (arr: Option<'A> [,]) =
    init (Array2D.length1 arr) (Array2D.length2 arr) (fun x y -> arr[x, y])


let ofVertList (verts: list<int * int * 'A>) (length1: int) (length2: int) =
    let toTuple (a, b, c) = ((a, b), c)

    let dict = new Dictionary<int * int, 'A>()

    for i in 0 .. verts.Length - 1 do
        dict.Add(toTuple verts[i])

    let initializer (x: int) (y: int) =
        if dict.ContainsKey((x + 1, y + 1)) then
            Some(dict[(x + 1, y + 1)])
        else
            None

    init length1 length2 initializer
