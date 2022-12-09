namespace DoMyHomework

open System
open Vector
open Matrix
open Multiply
open BinTree

module Main =

    [<EntryPoint>]
    let main argv =

        // let M = Matrix(array2D [[Some 1; Some 0; Some 1; Some 3; Some 2]
        //                         [Some 2; Some 1; Some -2; Some 3; Some 1]
        //                         [Some 3; Some 3; Some 1; Some -5; Some 5]
        //                         [Some 3; Some 3; None; Some 1; Some 0]
        //                         [None; None; Some 1; Some 4; None]])
        //
        // let M2 = Matrix(array2D [[Some 1; Some 0; Some 1; Some 3; Some 2]
        //                          [Some 2; Some 1; Some -2; Some 3; Some 1]
        //                          [Some 3; Some 3; Some 1; Some -5; Some 5]
        //                          [Some 3; Some 3; Some 2; Some 1; Some 0]
        //                          [Some 1; Some 9; Some 1; Some 4; Some 1]])
        //
        // let V = Vector([|Some 3; Some 0; Some -4; Some 2; Some 2|])
        //
        // let adjM = array2D [[false; true; true; true; false]
        //                     [false; false; false; true; false]
        //                     [false; false; false; true; true]
        //                     [false; false; false; false; false]
        //                     [false; true; false; false; false]]
        //
        // let front = [|true; false; false; false; false|]
        //
        // let mp a =
        //     if a then Some a
        //     else None
        //
        // let NoneFront = Array.map mp front
        // let NoneAdjM = Array2D.map mp adjM
        //
        // let treeAdjMN = Matrix(NoneAdjM)
        // let treeFrontN = Vector(NoneFront)
        //
        // let optAdjM = Array2D.map Some adjM
        // let optFront = Array.map Some front
        //
        // let treeAdjM = Matrix(optAdjM)
        // let treeFront = Vector(optFront)

        // printfn "%A\n" V.Data
        // printfn "%A" (naiveMultiply treeFrontN treeAdjMN (||) (&&)).Data

        let rnd = Random()
        let genRandomVector n = Array.init n (fun _ -> Some (rnd.Next(1, 5)))
        let genRandomNoneVector n =
            Array.init n (fun _ ->
                if rnd.Next(1, 3) = 2
                then None
                else Some (rnd.Next(-25, 25)))

        let genRandomMatrix x y = Array2D.init x y (fun _ _ -> Some (rnd.Next(1, 5)))
        let genRandomNoneMatrix x y =
            Array2D.init x y (fun _ _ ->
                if rnd.Next(1, 3) = 2
                then None
                else Some (rnd.Next(-25, 25)))


        // let len1 = rnd.Next(1, 11)
        // let len2 = rnd.Next(1, 11)
        //
        // let rv = genRandomNoneVector len1
        // let rm = genRandomNoneMatrix len1 len2
        //
        // let tv = Vector(rv)
        // let tm = Matrix(rm)
        //
        // printfn "vector: %A\nmatrix: \n%A" rv rm
        //
        // printfn "recursive: \n%A\nnaive: \n%A \nlen: %A"
        //     (vecMatMultiply tv tm (+) (*)).Data (naiveVecMatMultiply tv tm (+) (*)).Data (vecMatMultiply tv tm (+) (*)).Length

        // let arr = [|Some 4; Some 3|]
        // let arr2d = array2D [[Some 2; Some 2; Some 4]
        //                      [Some 1; Some 3; Some 4]]
        //
        // let vec = Vector(arr)
        // let mat = Matrix(arr2d)

        // printfn "%A" (expandBinTree vec.Data vec.Length (max mat.Length1 mat.Length2))

        // printfn "%A" (vecMatMultiply vec mat (+) (*)).Data
        // printfn "%A\n \n%A" vec.Data mat.Data

        // vecMatMultiply vec mat (+) (*) |> ignore

        let a = [|Some 4; Some 1; Some -1|]
        let tree = Vector(a).Data
        let b = expandBinTree tree a.Length (a.Length + 10)
        let c = cutBinTree b a.Length (a.Length + 10)

        printfn "%A" c
        0
