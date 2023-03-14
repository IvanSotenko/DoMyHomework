module DoMyHomework.Tests.ParallelOperationsTests

open DoMyHomework
open Expecto
open FsCheck
open DoMyHomework.RandomGeneration

open BinTree
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
        "1."
        [ testProperty "1"
          <| fun _ ->
              let len1, len2 = rnd.Next(1, 100), rnd.Next(1, 100)

              let tree1 = (genRandomNoneVector len1).Data
              let tree2 = (genRandomNoneVector len2).Data

              let actualResult = parallelAddBinTree1 tree1 tree2 addInt 2
              let expectedResult = addBinTree tree1 tree2 addInt

              Expect.equal actualResult expectedResult "the results were different"


          testProperty "vecMatMultiply is parallelVecMatMultiply"
          <| fun _ ->
              let len1, len2 = rnd.Next(1, 100), rnd.Next(1, 100)

              let mat = genRandomNoneMatrix len1 len2
              let vec = genRandomNoneVector len1

              let actualResult = vecMatMultiply vec mat addInt multInt
              let expectedResult = parallelVecMatMultiply vec mat addInt multInt 3

              Expect.equal actualResult.Data expectedResult.Data "the results were different"

          testCase "Empty vector multiplied by empty matrix is empty vector"
          <| fun _ ->
              let len1, len2 = 1, 100

              let mat = genRandomNoneMatrix len1 len2
              let vec = genRandomNoneVector len1

              let actualResult = vecMatMultiply vec mat addInt multInt
              let expectedResult = parallelVecMatMultiply vec mat addInt multInt 3

              Expect.equal actualResult.Data expectedResult.Data "the results were different" ]
