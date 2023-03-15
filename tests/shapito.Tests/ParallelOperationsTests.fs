﻿module DoMyHomework.Tests.ParallelOperationsTests

open DoMyHomework
open Expecto
open DoMyHomework.RandomGeneration

open BinTree
open Vector
open Matrix
open MatrixAlgebra

module OptionIntOperations =
    let addInt (a: Option<int>) (b: Option<int>) =
        match a, b with
        | Some x, Some y -> Some(x + y)
        | Some x, None -> Some x
        | None, Some x -> Some x
        | None, None -> None

    let multInt (a: Option<int>) (b: Option<int>) =
        match a, b with
        | Some x, Some y -> Some(x * y)
        | _ -> None

open OptionIntOperations

[<Tests>]
let multiplyTests =
    testList
        "Tests for MatrixAlgebra.parallelVecMatMultiply function"
        [ testProperty "parallelVecMatMultiply is vecMatMultiply (level of parallelism = 1)"
          <| fun _ ->
              let len1, len2 = rnd.Next(1, 25), rnd.Next(1, 25)

              let mat = genRandomNoneMatrix len1 len2
              let vec = genRandomNoneVector len1

              let actualResult = vecMatMultiply vec mat addInt multInt
              let expectedResult = parallelVecMatMultiply vec mat addInt multInt 1

              Expect.equal actualResult.Data expectedResult.Data "the results were different"


          testProperty "parallelVecMatMultiply is vecMatMultiply (level of parallelism = 3)"
          <| fun _ ->
              let len1, len2 = rnd.Next(1, 25), rnd.Next(1, 25)

              let mat = genRandomNoneMatrix len1 len2
              let vec = genRandomNoneVector len1

              let actualResult = vecMatMultiply vec mat addInt multInt
              let expectedResult = parallelVecMatMultiply vec mat addInt multInt 3

              Expect.equal actualResult.Data expectedResult.Data "the results were different"


          testCase "Empty vector multiplied by empty matrix is empty vector"
          <| fun _ ->
              let mat = Matrix(QTree.Empty, 0, 0)
              let vec = Vector(Empty, 0)

              let actualResult = (parallelVecMatMultiply vec mat addInt multInt 2).Data
              Expect.equal actualResult Empty "the results were different"


          testProperty "If length of vector dont match with length1 of matrix an exception is thrown"
          <| fun _ ->
              let matLen1, len2 = rnd.Next(2, 100), 1

              // the goal is to make (matLen1 <> vecLen)
              let vecLen =
                  if rnd.Next(1, 3) = 1 then
                      rnd.Next(1, matLen1)
                  else
                      rnd.Next(matLen1 + 1, 101)

              let mat = genRandomNoneMatrix matLen1 len2
              let vec = genRandomNoneVector vecLen

              Expect.throws
                  (fun _ -> parallelVecMatMultiply vec mat addInt multInt 2 |> ignore)
                  $"The dimensions of the matrix are incompatible
                    for multiplication with the dimensions of the vector:
                    vector length is {vec.Length} but matrix size is {mat.Length1}x{mat.Length2}"


          testProperty "Multiply function returns fully collapsed tree"
          <| fun _ ->
              let len1, len2 = rnd.Next(1, 100), rnd.Next(1, 100)

              let mat = genRandomNoneMatrix len1 len2
              let vec = genRandomNoneVector len1

              let actualResult = (parallelVecMatMultiply vec mat addInt multInt 2).Data
              let expectedResult = collapseBinTree actualResult

              Expect.equal actualResult expectedResult "the results were different" ]


[<Tests>]
let addBinTreeTests =
    testList
        "Tests for BinTree.parallelAddBinTree1 and BinTree.parallelAddBinTree2"
        [ testProperty "parallelAddBinTree2 is addBinTree (level of parallelism = 1)"
          <| fun _ ->
              let len = rnd.Next(1, 50)

              let tree1 = (genRandomNoneVector len).Data
              let tree2 = (genRandomNoneVector len).Data

              let res1 = parallelAddBinTree tree1 tree2 addInt 1
              let res2 = addBinTree tree1 tree2 addInt

              Expect.equal res1 res2 "the results were different"


          testProperty "parallelAddBinTree2 is addBinTree (level of parallelism = 3)"
          <| fun _ ->
              let len = rnd.Next(1, 50)

              let tree1 = (genRandomNoneVector len).Data
              let tree2 = (genRandomNoneVector len).Data

              let res1 = parallelAddBinTree tree1 tree2 addInt 3
              let res2 = addBinTree tree1 tree2 addInt

              Expect.equal res1 res2 "the results were different"


          testProperty "(AddBinTree tree emptyTree) is tree"
          <| fun _ ->
              let len = rnd.Next(1, 50)
              let tree = (genRandomNoneVector len).Data
              let emptyTree = Empty

              let actualResult = parallelAddBinTree tree emptyTree addInt 2

              Expect.equal actualResult tree "the results were different" ]