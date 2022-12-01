namespace DoMyHomework

open shapito
open QTree
open Vector
open Matrix
open Multiply
open shapito.BinTree
open shapito.QTree

module Main =

    [<EntryPoint>]
    let main argv =

        let test = Matrix(array2D [[1; 1; 1; 1; 9]
                                   [1; 1; 1; 1; 5]
                                   [1; 1; 1; 1; 1]
                                   [1; 1; 1; 1; 5]
                                   [3; 5; 3; 9; 2]])
        // printf "%A" test

        let m = Matrix(array2D [[4; 2; 1; 2]
                                [5; 1; 3; 9]
                                [5; 1; -2; -1]
                                [6; 2; 2; 2]])

        let v = Vector([|1; 1; 3; 5|])

        let E = Matrix(array2D [[1; 0; 0; 0]
                                [0; 1; 0; 0]
                                [0; 0; 1; 0]
                                [0; 0; 0; 1]])

        let v1 = Vector([|1; 1|])
        let m1 = Matrix(array2D [[1; 1]
                                 [1; 1]])

        // printfn "%A" (naiveMultiply testVector testMatrix (fun a b -> a + b) (fun a b -> a*b)).BinaryTree
        // printfn "%A" (MatXEl testMatrix 5 (*))

        let adventureTime (mat: Matrix<'A>) (vec: Vector<'A>) (plus: 'A -> 'A -> 'A) (mult: 'A -> 'A -> 'A) =
            let btree = vec.BinaryTree
            let qtree = mat.QuadTree

            let rec treePlus tree1 tree2 =
                match tree1, tree2 with
                | BinTree.Node (l1, r1), BinTree.Node(l2, r2) -> BinTree.Node(treePlus l1 l2, treePlus r1 r2)
                | BinTree.Leaf a, BinTree.Leaf b -> BinTree.Leaf (plus a b)
                | BinTree.Leaf a, BinTree.Node(l, r) -> BinTree.Node(treePlus (BinTree.Leaf a) l, treePlus (BinTree.Leaf a) r)
                | BinTree.Node(l, r), BinTree.Leaf a -> BinTree.Node(treePlus (BinTree.Leaf a) l, treePlus (BinTree.Leaf a) r)
                | _ ->
                    printfn $"tree1 = {tree1}\ntree2 = {tree2}"
                    failwith "oops"

            let rec core btree qtree =
                match btree, qtree with

                | BinTree.Node (l, r), Node(nw, ne, sw, se) ->
                    BinTree.Node(treePlus (core l nw) (core r sw), treePlus (core l ne) (core r se))

                | BinTree.Node (l, r), Leaf a ->
                    BinTree.Node(treePlus (core l (Leaf a)) (core r (Leaf a)), treePlus (core l (Leaf a)) (core r (Leaf a)))

                | BinTree.Node (l, r), Empty ->
                    BinTree.Node(treePlus (core l Empty) (core r Empty), treePlus (core l Empty) (core r Empty))

                | BinTree.Leaf a, Node(nw, ne, sw, se) ->
                    BinTree.Node(treePlus (core (BinTree.Leaf a) nw) (core (BinTree.Leaf a) sw), treePlus (core (BinTree.Leaf a) ne) (core (BinTree.Leaf a) se))

                | BinTree.Leaf a, Leaf b -> BinTree.Leaf (mult a b)

                | BinTree.Leaf _, Empty -> BinTree.Empty

                | BinTree.Empty, Node _ -> BinTree.Empty

                | BinTree.Empty, Leaf _ -> BinTree.Empty

                | BinTree.Empty, Empty -> BinTree.Empty

            core btree qtree


        let res = adventureTime m v (+) (*)
        let res2 = adventureTime E v (+) (*)

        printfn "%A" res
        0
