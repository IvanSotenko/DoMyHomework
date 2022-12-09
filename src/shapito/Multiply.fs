module DoMyHomework.Multiply

open Matrix
open Vector
open BinTree
open QTree


let naiveVecMatMultiply (vec: Vector<'A>) (mat: Matrix<'B>) (add: 'C -> 'C -> 'C) (multiply: 'A -> 'B -> 'C) =

    let plus a b =
        match a, b with
        | Some x, Some y -> Some(add x y)
        | Some x, None -> Some x
        | None, Some x -> Some x
        | None, None -> None

    let mult a b =
        match a, b with
        | Some x, Some y -> Some(multiply x y)
        | _ -> None

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
                resultVector[i] <- plus resultVector[i] (mult vec[j] mat[j, i])

        Vector(resultVector)


let vecMatMultiply (vec: Vector<'A>) (mat: Matrix<'B>) (add: 'C -> 'C -> 'C) (mult: 'A -> 'B -> 'C) =

    if vec.Length <> mat.Length1 then
        failwith
            $"The dimensions of the matrix are incompatible
              for multiplication with the dimensions of the vector:
              vector length is {vec.Length} but matrix size is {mat.Length1}x{mat.Length2}"

    let size = max mat.Length1 mat.Length2
    let bTree = expandBinTree vec.Data vec.Length size
    let qTree = mat.Data

    let rec multiplyCore bTree qTree =
        match bTree, qTree with

        | BinTree.Node (l, r), Node (nw, ne, sw, se) ->
            BinTree.Node(
                addBinTree (multiplyCore l nw) (multiplyCore r sw) add,
                addBinTree (multiplyCore l ne) (multiplyCore r se) add
            )

        | BinTree.Node (l, r), leafOrEmpty ->
            BinTree.Node(
                addBinTree (multiplyCore l leafOrEmpty) (multiplyCore r leafOrEmpty) add,
                addBinTree (multiplyCore l leafOrEmpty) (multiplyCore r leafOrEmpty) add
            )

        | leafOrEmpty, Node (nw, ne, sw, se) ->
            BinTree.Node(
                addBinTree (multiplyCore leafOrEmpty nw) (multiplyCore leafOrEmpty sw) add,
                addBinTree (multiplyCore leafOrEmpty ne) (multiplyCore leafOrEmpty se) add
            )

        | BinTree.Leaf a, Leaf b -> BinTree.Leaf(mult a b)

        | _ -> BinTree.Empty

    let rawRes = multiplyCore bTree qTree
    let collapsedRes = collapseBinTree rawRes
    let res = cutBinTree collapsedRes mat.Length2 size

    Vector(res, mat.Length2)
