namespace DoMyHomework

open shapito
open Vector
open Matrix
open Multiply

module Main =

    [<EntryPoint>]
    let main argv =

        let M = Matrix(array2D [[Some 1; Some 0; Some 1; Some 3; Some 2]
                                [Some 2; Some 1; Some -2; Some 3; Some 1]
                                [Some 3; Some 3; Some 1; Some -5; Some 5]
                                [Some 3; Some 3; None; Some 1; Some 0]
                                [None; None; Some 1; Some 4; None]])

        let M2 = Matrix(array2D [[Some 1; Some 0; Some 1; Some 3; Some 2]
                                 [Some 2; Some 1; Some -2; Some 3; Some 1]
                                 [Some 3; Some 3; Some 1; Some -5; Some 5]
                                 [Some 3; Some 3; Some 2; Some 1; Some 0]
                                 [Some 1; Some 9; Some 1; Some 4; Some 1]])

        let V = Vector([|Some 3; Some 0; Some -4; Some 2; Some 2|])

        let adjM = array2D [[false; true; true; true; false]
                            [false; false; false; true; false]
                            [false; false; false; true; true]
                            [false; false; false; false; false]
                            [false; true; false; false; false]]

        let front = [|true; false; false; false; false|]

        let mp a =
            if a then Some a
            else None

        let NoneFront = Array.map mp front
        let NoneAdjM = Array2D.map mp adjM

        let treeAdjMN = Matrix(NoneAdjM)
        let treeFrontN = Vector(NoneFront)

        let optAdjM = Array2D.map Some adjM
        let optFront = Array.map Some front

        let treeAdjM = Matrix(optAdjM)
        let treeFront = Vector(optFront)

        // printfn "%A\n" V.Data
        printfn "%A" (naiveMultiply treeFrontN treeAdjMN (||) (&&)).Data
        0
