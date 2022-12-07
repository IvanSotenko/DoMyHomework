module shapito.Multiply
open Matrix
open Vector
open BinTree
open QTree

let naiveMultiply (vec: Vector<'A>) (mat: Matrix<'A>) (add: 'A -> 'A -> 'A) (multiply: 'A -> 'A -> 'A) =

    let plus a b =
        match a, b with
        | Some x, Some y -> Some (add x y)
        | Some x, None -> Some x
        | None, Some x -> Some x
        | None, None -> None

    let multy a b =
        match a, b with
        | Some x, Some y -> Some (add x y)
        | _ -> None

    if vec.Length <> mat.Length1 then failwith $"The dimensions of the matrix are incompatible
                                                 for multiplication with the dimensions of the vector:
                                                 vector length is {vec.Length} but matrix size is {mat.Length1}x{mat.Length2}"
    else
        let columns = mat.Length2
        let len = vec.Length

        let mutable newVector = [||]
        for i in 0 .. (columns - 1) do
            let mutable el = multy (vec.getItem 0) (mat.getItem (0, i))
            for j in 1 .. (len - 1) do
                el <- plus el (multy (vec.getItem j) (mat.getItem (j, i)))
            newVector <- Array.append newVector [|el|]

        Vector(newVector)


let addTree tree1 tree2 addFunc =

    let rec treePlusSub tree1 tree2 =
        match tree1, tree2 with
        | BinTree.Node (l1, r1), BinTree.Node(l2, r2) -> BinTree.Node(treePlusSub l1 l2, treePlusSub r1 r2)
        | BinTree.Node (l, r), leafOrEmpty -> BinTree.Node(treePlusSub l leafOrEmpty, treePlusSub r leafOrEmpty)
        | leafOrEmpty, BinTree.Node (l, r) -> BinTree.Node(treePlusSub l leafOrEmpty, treePlusSub r leafOrEmpty)
        | BinTree.Leaf a, BinTree.Leaf b -> BinTree.Leaf (addFunc a b)
        | leafOrEmpty, BinTree.Empty -> leafOrEmpty
        | BinTree.Empty, leafOrEmpty -> leafOrEmpty

    treePlusSub tree1 tree2

let multiply (vec: Vector<'A>) (mat: Matrix<'A>) (add: 'A -> 'A -> 'A) (mult: 'A -> 'A -> 'A) =

    if vec.Length <> mat.Length1 then failwith $"The dimensions of the matrix are incompatible
                                                 for multiplication with the dimensions of the vector:
                                                 vector length is {vec.Length} but matrix size is {mat.Length1}x{mat.Length2}"

    let bTree = vec.Data
    let qTree = mat.Data

    let rec core bTree qTree =
        match bTree, qTree with

        | BinTree.Node (l, r), Node(nw, ne, sw, se) ->
            BinTree.Node(addTree (core l nw) (core r sw) add, addTree (core l ne) (core r se) add)

        | BinTree.Node (l, r), qTree ->
            BinTree.Node(addTree (core l qTree) (core r qTree) add, addTree (core l qTree) (core r qTree) add)

        | btree, Node(nw, ne, sw, se) ->
            BinTree.Node(addTree (core btree nw) (core btree sw) add, addTree (core btree ne) (core btree se) add)

        | BinTree.Leaf a, Leaf b -> BinTree.Leaf (mult a b)

        | _ -> BinTree.Empty

    Vector(collapseBinTree (core bTree qTree), vec.Length)
