module DoMyHomework.Tests.MatrixOperationsTests

open System
open DoMyHomework
open Expecto
open FsCheck

open Matrix
open Vector
open Multiply

let config = { Config.Default with MaxTest = 10000 }
let rnd = Random()
let getRandomVector n = Array.init n (fun _ -> Some (rnd.Next(-25, 25)))
let getRandomNoneVector n =
    Vector(Array.init n (fun _ ->
        if rnd.Next(1, 5) = 5
        then None
        else Some (rnd.Next(-25, 25))))

let getRandomMatrix x y = Array2D.init x y (fun _ _ -> Some (rnd.Next(-25, 25)))
let getRandomNoneMatrix x y =
    Matrix(Array2D.init x y (fun _ _ ->
        if rnd.Next(1, 5) = 5
        then None
        else Some (rnd.Next(-25, 25))))


[<Tests>]
let vectorMatrixMultiplyTests =

    testList
        "Test for Multiply.multiply func"
        [
            testProperty "something"
            <| fun _ ->

                let len1, len2 = rnd.Next(1, 100), rnd.Next(1, 100)
                let mat = getRandomNoneMatrix len1 len2
                let vec = getRandomNoneVector len1

                let arr2d = getRandomMatrix len1 len2
                let arr = getRandomVector len1

                let vec = Vector(arr)
                let mat = Matrix(arr2d)

                let actual_result = vecMatMultiply vec mat (+) (*)
                let expected_result = naiveVecMatMultiply vec mat (+) (*)

                Expect.equal actual_result.Data expected_result.Data "Something is wrong here"


                // let i, j = rnd.Next(1, 10), rnd.Next(1, 10)
                // let a = getRandomMatrix i j
                // let b = Matrix(a)
                //
                // let ind1, ind2 = rnd.Next(0, i - 1), rnd.Next(0, j - 1)
                //
                // Expect.equal b[ind1, ind2] a[ind1, ind2] (sprintf "\n%A \n %A %A -- %A %A" a i j ind1 ind2)
        //         let i = rnd.Next(1, 100)
        //         let a = getRandomVector i
        //         let b = Vector(a)
        //
        //         let ind = rnd.Next(0, i - 1)
        //
        //         Expect.equal a[ind] b[ind] "oh yeah"
        ]

