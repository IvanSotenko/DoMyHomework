namespace DoMyHomework

open shapito
open QTree
open Vector
open Matrix
open Multiply
open shapito.BinTree

module Main =

    [<EntryPoint>]
    let main argv =

        let test = Matrix(array2D [[1; 1; 1; 1; 9]
                                   [1; 1; 1; 1; 5]
                                   [1; 1; 1; 1; 1]
                                   [1; 1; 1; 1; 5]
                                   [3; 5; 3; 9; 2]])
        // printf "%A" test

        let testMatrix = Matrix(array2D [[4; 2; 1; 2]
                                         [5; 1; 3; 9]
                                         [5; 1; -2; -1]
                                         [6; 2; 2; 2]])

        let testVector = Vector([|1; -3; 1; 5|])

        let testVectorMini = Vector([|1; 4|])
        let testMatrixMini = Matrix(array2D [[1; -3]
                                             [1; 2]])

        // printfn "%A" (naiveMultiply testVector testMatrix (fun a b -> a + b) (fun a b -> a*b)).BinaryTree
        // printfn "%A" (MatXEl testMatrix 5 (*))

        let adventureTime (mat: Matrix<'A>) (vec: Vector<'A>) (plus: 'A -> 'A -> 'A) (mult: 'A -> 'A -> 'A) =
            let btree = vec.BinaryTree
            let qtree = mat.QuadTree

            let rec treePlus tree1 tree2 =
                match tree1, tree2 with
                | Node (l1, r1), Node(l2, r2) -> Node(treePlus l1 l2, treePlus r1 r2)
                | Leaf a, Leaf b -> Leaf (plus a b)
                | _ -> failwith "oops"

            let rec core btree qtree =
                match btree, qtree with
                | Node (l, r), QTree.Node(nw, ne, sw, se) ->
                    Node(treePlus (core l nw) (core r sw), treePlus (core l ne) (core r se))
                | Leaf a, QTree.Leaf b -> Leaf (mult a b)
                | _ -> failwith "oops"

            core btree qtree


        let res = adventureTime testMatrix testVector (+) (fun a b -> a*b)

        printfn "%A" res
        0
