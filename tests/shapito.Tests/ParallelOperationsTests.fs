module DoMyHomework.Tests.ParallelOperationsTests

open DoMyHomework
open Expecto

open BinTree
open Vector
open Matrix
open MatrixAlgebra
open OptionIntOperations
open Generators

[<Tests>]
let multiplyTests =
    testList
        "Tests for MatrixAlgebra.vecMatMultiply function with nonzero pLevel parameter"
        [ testPropertyWithConfig config "parallelVecMatMultiply is vecMatMultiply (level of parallelism = 1)"
          <| fun (pack: MultipliableVectorAndMatrix<int>) ->

              let actualResult = vecMatMultiply pack.Vector pack.Matrix addInt multInt 0
              let expectedResult = vecMatMultiply pack.Vector pack.Matrix addInt multInt 1

              Expect.equal actualResult.Data expectedResult.Data "the results were different"


          testPropertyWithConfig config "parallelVecMatMultiply is vecMatMultiply (level of parallelism = 3)"
          <| fun (pack: MultipliableVectorAndMatrix<int>) ->

              let actualResult = vecMatMultiply pack.Vector pack.Matrix addInt multInt 0
              let expectedResult = vecMatMultiply pack.Vector pack.Matrix addInt multInt 3

              Expect.equal actualResult.Data expectedResult.Data "the results were different"


          testCase "Empty vector multiplied by empty matrix is empty vector"
          <| fun _ ->
              let mat = Matrix(QTree.Empty, 0, 0)
              let vec = Vector(Empty, 0)

              let actualResult = (vecMatMultiply vec mat addInt multInt 2).Data

              Expect.equal actualResult Empty "the results were different"


          testPropertyWithConfig config "If length of vector dont match with length1 of matrix an exception is thrown"
          <| fun (pack: UnmatchedVectorAndMatrix<_>) ->

              let mat = pack.Matrix
              let vec = pack.Vector

              Expect.throws
                  (fun _ -> vecMatMultiply vec mat addInt multInt 2 |> ignore)
                  $"The dimensions of the matrix are incompatible
                    for multiplication with the dimensions of the vector:
                    vector length is {vec.Length} but matrix size is {mat.Length1}x{mat.Length2}"


          testPropertyWithConfig config "Multiply function returns fully collapsed tree"
          <| fun (pack: MultipliableVectorAndMatrix<int>) ->

              let actualResult =
                  (vecMatMultiply pack.Vector pack.Matrix addInt multInt 2)
                      .Data

              let expectedResult = collapseBinTree actualResult

              Expect.equal actualResult expectedResult "the results were different" ]


[<Tests>]
let addBinTreeTests =
    testList
        "Tests for BinTree.parallelAddBinTree"
        [ testPropertyWithConfig config "parallelAddBinTree2 is addBinTree (level of parallelism = 1)"
          <| fun (tree1: CollapsedBinTree<int>) (tree2: CollapsedBinTree<int>) ->

              let res1 = parallelAddBinTree tree1.Get tree2.Get addInt 1
              let res2 = addBinTree tree1.Get tree2.Get addInt

              Expect.equal res1 res2 "the results were different"


          testPropertyWithConfig config "parallelAddBinTree2 is addBinTree (level of parallelism = 3)"
          <| fun (tree1: CollapsedBinTree<int>) (tree2: CollapsedBinTree<int>) ->

              let res1 = parallelAddBinTree tree1.Get tree2.Get addInt 3
              let res2 = addBinTree tree1.Get tree2.Get addInt

              Expect.equal res1 res2 "the results were different"


          testPropertyWithConfig config "(AddBinTree tree emptyTree) is tree"
          <| fun (tree: CollapsedBinTree<int>) ->

              let emptyTree = Empty
              let actualResult = parallelAddBinTree tree.Get emptyTree addInt 2

              Expect.equal actualResult tree.Get "the results were different" ]
