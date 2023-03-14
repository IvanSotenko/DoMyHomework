module DoMyHomework.MatrixAlgebra

open Matrix
open Vector
open BinTree
open QTree

let naiveVecMatMultiply
    (vec: Vector<'A>)
    (mat: Matrix<'B>)
    (add: Option<'C> -> Option<'C> -> Option<'C>)
    (mult: Option<'A> -> Option<'B> -> Option<'C>)
    =

    if vec.Length <> mat.Length1 then
        failwith
            $"The dimensions of the matrix are incompatible
              for multiplication with the dimensions of the vector:
              vector length is {vec.Length} but matrix size is {mat.Length1}x{mat.Length2}"
    else

        let len1 = mat.Length1
        let len2 = mat.Length2

        let mutable resultVector = Array.create len2 None

        for i in 0 .. (len2 - 1) do
            for j in 0 .. (len1 - 1) do
                resultVector[i] <- add resultVector[i] (mult vec[j] mat[j, i])

        Vector(resultVector)


let multiplyCore bTree qTree level add mult =
    let rec multiplyCoreSub bTree qTree level =

        if level = 0 then

            match bTree, qTree with
            | leafOrEmpty1, leafOrEmpty2 ->
                (mult (BinTreeToOption leafOrEmpty1) (QTreeToOption leafOrEmpty2))
                |> optionToBinTree

        else
            match bTree, qTree with

            | BinTree.Node (l, r), Node (nw, ne, sw, se) ->
                BinTree.Node(
                    (addBinTree (multiplyCoreSub l nw (level - 1)) (multiplyCoreSub r sw (level - 1)) add)
                    |> binCollapse,
                    (addBinTree (multiplyCoreSub l ne (level - 1)) (multiplyCoreSub r se (level - 1)) add)
                    |> binCollapse
                )
                |> binCollapse

            | BinTree.Node (l, r), leafOrEmpty ->
                BinTree.Node(
                    (addBinTree (multiplyCoreSub l leafOrEmpty (level - 1)) (multiplyCoreSub r leafOrEmpty (level - 1)) add)
                    |> binCollapse,
                    (addBinTree (multiplyCoreSub l leafOrEmpty (level - 1)) (multiplyCoreSub r leafOrEmpty (level - 1)) add)
                    |> binCollapse
                )
                |> binCollapse

            | leafOrEmpty, Node (nw, ne, sw, se) ->
                BinTree.Node(
                    (addBinTree (multiplyCoreSub leafOrEmpty nw (level - 1)) (multiplyCoreSub leafOrEmpty sw (level - 1)) add)
                    |> binCollapse,
                    (addBinTree (multiplyCoreSub leafOrEmpty ne (level - 1)) (multiplyCoreSub leafOrEmpty se (level - 1)) add)
                    |> binCollapse
                )
                |> binCollapse

            | leafOrEmptyBin, leafOrEmptyQ ->
                let summand = (multiplyCoreSub leafOrEmptyBin leafOrEmptyQ (level - 1))
                let tree = addBinTree summand summand add |> binCollapse
                BinTree.Node(tree, tree) |> binCollapse

    multiplyCoreSub bTree qTree level


let vecMatMultiply
    (vec: Vector<'A>)
    (mat: Matrix<'B>)
    (add: Option<'C> -> Option<'C> -> Option<'C>)
    (mult: Option<'A> -> Option<'B> -> Option<'C>)
    =

    if vec.Length <> mat.Length1 then
        failwith
            $"The dimensions of the matrix are incompatible
              for multiplication with the dimensions of the vector:
              vector length is {vec.Length} but matrix size is {mat.Length1}x{mat.Length2}"

    let size = max mat.Length1 mat.Length2

    let depth =
        if size = 1 || size = 0 then
            0
        else
            int (System.Math.Ceiling(System.Math.Log(size, 2)))

    let qTree = mat.Data
    let binTree = expandBinTree vec.Data vec.Length size
    let rawRes = multiplyCore binTree qTree depth add mult
    let res = cutBinTree rawRes mat.Length2 size

    Vector(res, mat.Length2)


let parallelVecMatMultiply
    (vec: Vector<'A>)
    (mat: Matrix<'B>)
    (add: Option<'C> -> Option<'C> -> Option<'C>)
    (mult: Option<'A> -> Option<'B> -> Option<'C>)
    (pLevel: int)
    =

    if vec.Length <> mat.Length1 then
        failwith
            $"The dimensions of the matrix are incompatible
              for multiplication with the dimensions of the vector:
              vector length is {vec.Length} but matrix size is {mat.Length1}x{mat.Length2}"

    let size = max mat.Length1 mat.Length2

    let depth =
        if size = 1 || size = 0 then
            0
        else
            int (System.Math.Ceiling(System.Math.Log(size, 2)))

    let qTree = mat.Data
    let binTree = expandBinTree vec.Data vec.Length size

    let rec core bTree qTree treeLevel pLevel =

        let parallelCompute (l, r) (nw, ne, sw, se) pLevel =
            let tasks = [async { return (addBinTree (core l nw (treeLevel - 1) (pLevel - 1)) (core r sw (treeLevel - 1) (pLevel - 1)) add) |> binCollapse }
                         async { return (addBinTree (core l ne (treeLevel - 1) (pLevel - 1)) (core r se (treeLevel - 1) (pLevel - 1)) add) |> binCollapse }]

            let nodes = tasks |> Async.Parallel |> Async.RunSynchronously
            BinTree.Node(nodes[0], nodes[1]) |> binCollapse


        if pLevel = 0 then
            multiplyCore bTree qTree treeLevel add mult

        elif treeLevel = 0 then

            match bTree, qTree with
            | leafOrEmpty1, leafOrEmpty2 ->
                (mult (BinTreeToOption leafOrEmpty1) (QTreeToOption leafOrEmpty2))
                |> optionToBinTree

        else
            match bTree, qTree with

            | BinTree.Node (l, r), Node (nw, ne, sw, se) -> parallelCompute (l, r) (nw, ne, sw, se) pLevel
            | BinTree.Node (l, r), leafOrEmpty -> parallelCompute (l, r) (leafOrEmpty, leafOrEmpty, leafOrEmpty, leafOrEmpty) pLevel
            | leafOrEmpty, Node (nw, ne, sw, se) -> parallelCompute (leafOrEmpty, leafOrEmpty) (nw, ne, sw, se) pLevel
            | leafOrEmptyBin, leafOrEmptyQ  ->
                let summand = (multiplyCore leafOrEmptyBin leafOrEmptyQ (treeLevel - 1) add mult)
                let tree = addBinTree summand summand add |> binCollapse
                BinTree.Node(tree, tree) |> binCollapse

    let rawRes = core binTree qTree depth pLevel
    let res = cutBinTree rawRes mat.Length2 size

    Vector(res, mat.Length2)
