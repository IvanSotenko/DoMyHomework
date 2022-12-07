module shapito.Multiply
open Matrix
open Vector
open BinTree
open QTree

// let naiveMultiply (vec: Vector<'A>) (mat: Matrix<'A>) (plus: 'A -> 'A -> 'A) (mult: 'A -> 'A -> 'A) =
//
//     if vec.Length <> mat.Length1 then failwith $"The dimensions of the matrix are incompatible
//                                                  for multiplication with the dimensions of the vector:
//                                                  vector length is {vec.Length} but matrix size is {mat.Length1}x{mat.Length2}"
//     else
//         let columns = mat.actualLen2
//         let len = vec.actualLength
//
//         let mutable newVector = [||]
//         for i in 0 .. (columns - 1) do
//
//             let m1, m2 =
//                 match (vec.getItem 0), (mat.getItem (0, i)) with
//                 | Some a, Some b -> a, b
//                 | _ -> failwith "some exception"
//
//             let mutable el = multiply m1 m2
//
//             for j in 1 .. (len - 1) do
//
//                 let m1, m2 =
//                     match (vec.getItem j), (mat.getItem (j, i)) with
//                     | Some a, Some b -> a, b
//                     | _ -> failwith "some exception"
//
//                 el <- add el (multiply m1 m2)
//
//             newVector <- Array.append newVector [|el|]
//
//         Vector(newVector)


let addTree tree1 tree2 addFunc =

    let rec treePlusSub tree1 tree2 =
        match tree1, tree2 with
        | BinTree.Node (l1, r1), BinTree.Node(l2, r2) -> BinTree.Node(treePlusSub l1 l2, treePlusSub r1 r2)
        | BinTree.Node (l, r), tree -> BinTree.Node(treePlusSub l tree, treePlusSub r tree)
        | tree, BinTree.Node (l, r) -> BinTree.Node(treePlusSub l tree, treePlusSub r tree)
        | BinTree.Leaf a, BinTree.Leaf b -> BinTree.Leaf (addFunc a b)
        | leafOrEmpty, BinTree.Empty -> leafOrEmpty
        | BinTree.Empty, leafOrEmpty -> leafOrEmpty

    treePlusSub tree1 tree2

let multiply (vec: Vector<'A>) (mat: Matrix<'A>) (plus: 'A -> 'A -> 'A) (mult: 'A -> 'A -> 'A) =

    if vec.Length <> mat.Length1 then failwith $"The dimensions of the matrix are incompatible
                                                 for multiplication with the dimensions of the vector:
                                                 vector length is {vec.Length} but matrix size is {mat.Length1}x{mat.Length2}"

    let bTree = vec.Data
    let qTree = mat.Data

    printfn "%A\n \n%A" bTree qTree

    let rec core bTree qTree =
        match bTree, qTree with

        | BinTree.Node (l, r), Node(nw, ne, sw, se) ->
            BinTree.Node(addTree (core l nw) (core r sw) plus, addTree (core l ne) (core r se) plus)

        | BinTree.Node (l, r), qTree ->
            BinTree.Node(addTree (core l qTree) (core r qTree) plus, addTree (core l qTree) (core r qTree) plus)

        | btree, Node(nw, ne, sw, se) ->
            BinTree.Node(addTree (core btree nw) (core btree sw) plus, addTree (core btree ne) (core btree se) plus)

        | BinTree.Leaf a, Leaf b -> BinTree.Leaf (mult a b)

        | _ -> BinTree.Empty

    Vector(collapseBinTree (core bTree qTree), vec.Length)
