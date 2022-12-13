namespace DoMyHomework

open Vector
open BinTree
open Matrix
open System
open QTree
open Multiply

module randomGenerations =
    let rnd = Random()

    let genRandomArray n =
        Array.init n (fun _ -> Some(rnd.Next(-10, 10)))

    let genRandomNoneArray n =
        Array.init n (fun _ ->
            if rnd.Next(1, 5) = 4 then
                None
            else
                Some(rnd.Next(-10, 10)))

    let genRandomVector n = Vector(genRandomArray n)
    let genRandomNoneVector n = Vector(genRandomNoneArray n)


    let genRandomArray2D x y =
        Array2D.init x y (fun _ _ -> Some(rnd.Next(-10, 10)))

    let genRandomNoneArray2D x y =
        Array2D.init x y (fun _ _ ->
            if rnd.Next(1, 5) = 4 then
                None
            else
                Some(rnd.Next(-10, 10)))

    let genRandomMatrix x y = Matrix(genRandomArray2D x y)
    let genRandomNoneMatrix x y = Matrix(genRandomNoneArray2D x y)

module OptionIntOperations =
    let addInt (a: Option<int>) (b: Option<int>) =
        match a, b with
        | Some x, Some y -> Some (x + y)
        | Some x, None -> Some x
        | None, Some x -> Some x
        | None, None -> None

    let multInt (a: Option<int>) (b: Option<int>): Option<int> =
        match a, b with
        | Some x, Some y -> Some (x * y)
        | _ -> None


module Restoring =
    let restoreArray (vec: Vector<_>) = Array.init vec.Length (fun i -> vec[i])

    let restoreArray2D (mat: Matrix<_>) =
        Array2D.init mat.Length1 mat.Length2 (fun i j -> mat[i, j])


open Restoring
open OptionIntOperations
open randomGenerations

module Main =

    [<EntryPoint>]
    let main argv =

          // let l1, l2 = rnd.Next(1, 20), rnd.Next(1, 20)
          // let l1, l2 = rnd.Next(1, 10000), 1
          // let a = genRandomNoneArray l1
          // let b = genRandomNoneArray2D l1 l2

          // let vec = Vector(a)
          // let mat = Matrix(b)

          // printfn "%A\n%A\nlen = %A" a vec.Data vec.Length

          // printfn "vector\n%A\n \nmatrix\n%A\n \nrecurcive\n%A\n \nnaive\n%A\n \nlen = %A\nequality - %A"
          //     a b
          //     (vecMatMultiply vec mat addInt multInt).Data
          //     (naiveVecMatMultiply vec mat addInt multInt).Data
          //     (vecMatMultiply vec mat addInt multInt).Length
          //     ((vecMatMultiply vec mat addInt multInt).Data = (naiveVecMatMultiply vec mat addInt multInt).Data)


          // let l1, l2 = rnd.Next(1, 20), rnd.Next(1, 20)
          // let a = genRandomArray l1
          // let b = genRandomArray2D l1 l2

          // let vec = Vector(Array.init 4 (fun i -> Some a[i]))
          // let mat = Matrix(Array2D.init 4 4 (fun i j -> Some b[i, j]))

          // let vec = Vector(a)
          // let mat = Matrix(b)

          // printf "%A" mat.Data
          // vecMatMultiply vec mat addInt multInt |> ignore
          // printfn "%A" mat.Data
          // printf "%A" (vecMatMultiply vec mat addInt multInt).Data

          // let len = 9

          // let arr1 = [|Some 1; None; Some 1; None; Some 1; None; Some 1; None|]
          //
          // // let arr1 = genRandomNoneArray len
          // let vec = Vector(arr1)
          // let arr2 = restoreArray vec

          // printfn "%A\n \n%A\n \n%A\n \nequality - %A" arr1 arr2 vec.Data (arr1 = arr2)

          let a = [||]
          let b = array2D []


          // let a = [|1; 2; 3; 4|]
          // let b = array2D [[1]; [2]; [3]; [4]]

          let vec = Vector(Array.init a.Length (fun i -> Some a[i]))
          let mat = Matrix(Array2D.init (Array2D.length1 b) (Array2D.length2 b) (fun i j -> Some b[i, j]))
          //
          printfn "%A" (vecMatMultiply vec mat addInt multInt).Data

          0
