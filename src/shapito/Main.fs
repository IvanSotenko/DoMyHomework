namespace DoMyHomework

open Vector
open Matrix
open BinTree
open BFS
open MatrixAlgebra

module Restoring =
    let restoreArray (vec: Vector<_>) = Array.init vec.Length (fun i -> vec[i])

    let restoreArray2D (mat: Matrix<_>) =
        Array2D.init mat.Length1 mat.Length2 (fun i j -> mat[i, j])


open Restoring
module Main =
    [<EntryPoint>]

    let main argv =

        let testData1 = [(1, 2, 19); (1, 5, 8); (1, 4, 11); (5, 2, 3); (5, 4, 10); (4, 1, 25); (5, 5, 1)]
        let testData2 = [(1, 2, 1); (1, 3, 1); (1, 4, 1); (2, 4, 1); (2, 3, 1); (3, 1, 1); (4, 3, 1); (3, 4, 1)]
        let testData3 = [(1, 1, 1); (1, 2, 1); (2, 1, 1); (2, 2, 1)]
        let testData4 = [(1, 1, 1); (0, 1, 1); (1, 1, 2); (1, 1, 1)]

        let testData6 = [(1, 2, 4); (1, 3, 2); (2, 4, 8); (2, 5, 2); (3, 2, 1); (3, 4, 4); (5, 4, 1)]
        let testStart = [|Some 0; None; None; None; None; None|]

        let testDataUSE = [(1, 2, 4); (2, 3, 6); (2, 4, 3); (2, 5, 6); (3, 5, 4); (4, 5, 2); (5, 6, 5)
                           (2, 1, 4); (3, 2, 6); (4, 2, 3); (5, 2, 6); (5, 3, 4); (5, 4, 2); (6, 5, 2)]

        let st = Vector(testStart)
        let mat = Matrix(testDataUSE, 6, 6)

        let a = BFS st mat

        // let res = vecMatMultiply st mat min add

        // printfn "%A" res.Data
        printfn "%A" a.Data

        // let vec = Vector([|None; Some 4; Some 2; None; None|])
        // let res = vecMatMultiply vec mat min add

        // printfn "%A" res.Data

        // let tree = Node (Node (Node (Empty, Leaf 4), Node (Leaf 2, Empty)), Empty)
        // let F = Vector(tree, 5)
        //
        //
        // let res = vecMatMultiply F mat min add

        // printfn "%A" res.Data
        0
