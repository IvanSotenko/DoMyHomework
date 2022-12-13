module DoMyHomework.Multiply

open Matrix
open Vector
open BinTree
open QTree

let naiveVecMatMultiply
    (vec: Vector<'A>)
    (mat: Matrix<'B>)
    (add: Option<'C> -> Option<'C> -> Option<'C>)
    (mult: Option<'A> -> Option<'B> -> Option<'C>) =

    if vec.Length <> mat.Length1 then
        failwith
            $"The dimensions of the matrix are incompatible
                                                 for multiplication with the dimensions of the vector:
                                                 vector length is {vec.Length} but matrix size is {mat.Length1}x{mat.Length2}"
    else

        let len1 = mat.Length1
        let len2 = mat.Length2

        let mutable resultVector = Array.init len2 (fun _ -> None)

        for i in 0 .. (len2 - 1) do
            for j in 0 .. (len1 - 1) do
                resultVector[i] <- add resultVector[i] (mult vec[j] mat[j, i])

        Vector(resultVector)


let collapsingVecMatMultiply
    (vec: Vector<'A>)
    (mat: Matrix<'B>)
    (add: Option<'C> -> Option<'C> -> Option<'C>)
    (mult: Option<'A> -> Option<'B> -> Option<'C>) =

    let collapsedNode (left, right) =
        if left = right then left
        else BinTree.Node (left, right)

    if vec.Length <> mat.Length1 then
        failwith
            $"The dimensions of the matrix are incompatible
              for multiplication with the dimensions of the vector:
              vector length is {vec.Length} but matrix size is {mat.Length1}x{mat.Length2}"

    let size = max mat.Length1 mat.Length2
    let bTree = expandBinTree vec.Data vec.Length size
    let qTree = mat.Data

    printfn "%A\n \n%A" bTree qTree

    let rec multiplyCore bTree qTree =
        match bTree, qTree with

        | BinTree.Node (l, r), Node (nw, ne, sw, se) ->
            // printfn "======\n%A\n%A" 1 1// (BinTree.Node (l, r)) (Node (nw, ne, sw, se))

            printfn "======\n%A\n%A\n%A\n%A\n======"
                (multiplyCore l nw) (multiplyCore r sw)
                (multiplyCore l ne) (multiplyCore r se)

            collapsedNode
                ((addBinTree (multiplyCore l nw) (multiplyCore r sw) add) |> collapseBinTree,
                (addBinTree (multiplyCore l ne) (multiplyCore r se) add) |> collapseBinTree)

        | BinTree.Node (l, r), leafOrEmpty ->
            collapsedNode
                ((addBinTree (multiplyCore l leafOrEmpty) (multiplyCore r leafOrEmpty) add) |> collapseBinTree,
                (addBinTree (multiplyCore l leafOrEmpty) (multiplyCore r leafOrEmpty) add) |> collapseBinTree)

        | leafOrEmpty, Node (nw, ne, sw, se) ->
            collapsedNode
                ((addBinTree (multiplyCore leafOrEmpty nw) (multiplyCore leafOrEmpty sw) add) |> collapseBinTree,
                (addBinTree (multiplyCore leafOrEmpty ne) (multiplyCore leafOrEmpty se) add) |> collapseBinTree)

        | leafOrEmpty1, leafOrEmpty2 -> (mult (BinTreeToOption leafOrEmpty1) (QTreeToOption leafOrEmpty2)) |> OptionToBinTree

    let rawRes = multiplyCore bTree qTree
    printfn "\n%A" rawRes
    let res = cutBinTree rawRes mat.Length2 size

    Vector(res, mat.Length2)


let vecMatMultiply
    (vec: Vector<'A>)
    (mat: Matrix<'B>)
    (add: Option<'C> -> Option<'C> -> Option<'C>)
    (mult: Option<'A> -> Option<'B> -> Option<'C>) =

    if vec.Length <> mat.Length1 then
        failwith
            $"The dimensions of the matrix are incompatible
              for multiplication with the dimensions of the vector:
              vector length is {vec.Length} but matrix size is {mat.Length1}x{mat.Length2}"

    let size = max mat.Length1 mat.Length2
    let depth = if size = 1 || size = 0 then 0 else int (System.Math.Ceiling(System.Math.Log(size, 2)))
    let qTree = mat.Data

    let binTree = expandBinTree vec.Data vec.Length size

    // printfn "depth = %A\n \nqTree\n%A\n \nbinTree\n%A" depth qTree binTree

    let rec multiplyCore bTree qTree level =
        if level = 0 then

            match bTree, qTree with
            | leafOrEmpty1, leafOrEmpty2 -> (mult (BinTreeToOption leafOrEmpty1) (QTreeToOption leafOrEmpty2)) |> OptionToBinTree

        else
            match bTree, qTree with

            | BinTree.Node (l, r), Node (nw, ne, sw, se) ->
                BinTree.Node(
                    addBinTree (multiplyCore l nw (level - 1)) (multiplyCore r sw (level - 1)) add,
                    addBinTree (multiplyCore l ne (level - 1)) (multiplyCore r se (level - 1)) add
                )

            | BinTree.Node (l, r), leafOrEmpty ->
                BinTree.Node(
                    addBinTree (multiplyCore l leafOrEmpty (level - 1)) (multiplyCore r leafOrEmpty (level - 1)) add,
                    addBinTree (multiplyCore l leafOrEmpty (level - 1)) (multiplyCore r leafOrEmpty (level - 1)) add
                )

            | leafOrEmpty, Node (nw, ne, sw, se) ->
                BinTree.Node(
                    addBinTree (multiplyCore leafOrEmpty nw (level - 1)) (multiplyCore leafOrEmpty sw (level - 1)) add,
                    addBinTree (multiplyCore leafOrEmpty ne (level - 1)) (multiplyCore leafOrEmpty se (level - 1)) add
                )

            | leafOrEmptyBin, leafOrEmptyQ ->
                let summand = (multiplyCore leafOrEmptyBin leafOrEmptyQ (level - 1))
                let tree = addBinTree summand summand add
                BinTree.Node(tree, tree)

    let rawRes = multiplyCore binTree qTree depth
    let collapsedRes = collapseBinTree rawRes
    let res = cutBinTree collapsedRes mat.Length2 size

    Vector(res, mat.Length2)
