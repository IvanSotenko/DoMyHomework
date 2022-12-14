namespace DoMyHomework

open Vector
open Matrix

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
        let testData4 = [(1, 1, 1); (1, 4, 2)]

        let mat = Matrix(testData4, 2, 2)

        let arr = restoreArray2D mat

        // printfn "%A" (divide [(1, 5, 8)] 2)

        printfn "%A" arr
        printfn "%A" mat.Data

        // let a = Matrix(array2D [[Some 1; Some 2]; [Some 3; Some 4]])
        //
        // printf "%A" (restoreArray2D a)
        0
