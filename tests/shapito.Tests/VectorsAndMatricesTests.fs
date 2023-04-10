module DoMyHomework.Tests.MatrixOperationsTests

open DoMyHomework
open Expecto
open FsCheck

open BinTree
open QTree
open Matrix
open Vector
open MatrixAlgebra
open Generators

module naiveConversions =
    let unpackOption x =
        match x with
        | Some a -> a
        | None -> failwith "Cant unpack None value"

    let vectorToArray (vec: Vector<_>) = Array.init vec.Length (fun i -> vec[i])

    let matrixToArray2D (mat: Matrix<_>) =
        Array2D.init mat.Length1 mat.Length2 (fun i j -> mat[i, j])

    let array2DToVertList (arr: Option<'A> [,]) =
        let mutable vertList = []

        for i in 0 .. (Array2D.length1 arr - 1) do
            for j in 0 .. (Array2D.length2 arr - 1) do
                let value = arr[i, j]

                if value <> None then
                    vertList <- List.append vertList [ (i + 1, j + 1, unpackOption value) ]

        vertList

    let arrayToUintList (arr: Option<'A> []) =
        let mutable uintList = []

        for i in 0 .. arr.Length - 1 do
            if arr[i] <> None then
                uintList <- List.append uintList [ uint (i + 1) ]

        uintList

open OptionIntOperations
open naiveConversions

[<Tests>]
let multiplyTests =
    testList
        "Tests for MatrixAlgebra.vecMatMultiply function"
        [ testPropertyWithConfig config "vecMatMultiply is naiveVecMatMultiply (without None)"
          <| fun (pack: MultipliableVectorAndMatrix<int>) ->

              let actualResult = vecMatMultiply pack.Vector pack.Matrix addInt multInt 0
              let expectedResult = naiveVecMatMultiply pack.Vector pack.Matrix addInt multInt

              Expect.equal actualResult.Data expectedResult.Data "the results were different"


          testPropertyWithConfig config "vecMatMultiply is naiveVecMatMultiply (with None)"
          <| fun (pack: MultipliableVectorAndMatrix<int>) ->

              let actualResult = vecMatMultiply pack.Vector pack.Matrix addInt multInt 0
              let expectedResult = naiveVecMatMultiply pack.Vector pack.Matrix addInt multInt

              Expect.equal actualResult.Data expectedResult.Data "the results were different"


          testCase "Empty vector multiplied by empty matrix is empty vector"
          <| fun _ ->
              let mat = Matrix(Empty, 0, 0)
              let vec = Vector(BinTree.Empty, 0)

              let actualResult = (vecMatMultiply vec mat addInt multInt 0).Data
              Expect.equal actualResult BinTree.Empty "the results were different"


          testPropertyWithConfig config "If length of vector dont match with length1 of matrix an exception is thrown"
          <| fun (pack: UnmatchedVectorAndMatrix<_>) ->

              let mat = pack.Matrix
              let vec = pack.Vector

              Expect.throws
                  (fun _ -> vecMatMultiply vec mat addInt multInt 0 |> ignore)
                  $"The dimensions of the matrix are incompatible
                    for multiplication with the dimensions of the vector:
                    vector length is {vec.Length} but matrix size is {mat.Length1}x{mat.Length2}"


          testPropertyWithConfig config "Multiply function returns fully collapsed tree"
          <| fun (pack: MultipliableVectorAndMatrix<int>) ->

              let actualResult =
                  (vecMatMultiply pack.Vector pack.Matrix addInt multInt 0)
                      .Data

              let expectedResult = collapseBinTree actualResult

              Expect.equal actualResult expectedResult "the results were different" ]


[<Tests>]
let vectorTypeTests =
    testList
        "Tests for Vector type"
        [
          // Index access
          testProperty "Index access test"
          <| fun (arr: NonEmptyArray<Option<int>>) ->

              let vec = Vector(arr.Get)
              let index = rnd.Next(0, arr.Get.Length)

              Expect.equal vec[index] arr.Get[index] "the results were different"


          // Vector constructors
          // Array vector constructor
          testProperty "Array constructor test (arr1 -> vec -> arr2) ==> (arr1 = arr2)"
          <| fun (arr: NonEmptyArray<Option<int>>) ->

              let vec = Vector(arr.Get)
              let actualResult = vectorToArray vec

              Expect.equal actualResult arr.Get "the results were different"


          testProperty "The array Vector constructor returns a fully collapsed tree"
          <| fun (arr: NonEmptyArray<Option<int>>) ->

              let actualResult = Vector(arr.Get).Data
              let expectedResult = collapseBinTree actualResult

              Expect.equal actualResult expectedResult "the results were different" ]


[<Tests>]
let matrixTypeTests =
    testList
        "Tests for Matrix type"
        [
          // Index access
          testPropertyWithConfig config "Index access test"
          <| fun (arr2D: NoneEmptyArray2D<Option<int>>) ->
              let mat = Matrix(arr2D.Get)
              let index1, index2 = rnd.Next(0, mat.Length1), rnd.Next(0, mat.Length2)

              Expect.equal mat[index1, index2] arr2D.Get[index1, index2] "the results were different"


          // Matrix constructors
          // Array2D constructor
          testPropertyWithConfig config "Array2D constructor tests (arr2D1 -> mat -> arr2D2) ==> (arr2D2 = arr2D2)"
          <| fun (arr2D: NoneEmptyArray2D<Option<int>>) ->

              let mat = Matrix(arr2D.Get)
              let actualResult = matrixToArray2D mat

              Expect.equal actualResult arr2D.Get "the results were different"


          testPropertyWithConfig config "Array2D Matrix constructor returns a fully collapsed tree"
          <| fun (arr2D: NoneEmptyArray2D<Option<int>>) ->

              let actualResult = Matrix(arr2D.Get).Data
              let expectedResult = collapseQTree actualResult

              Expect.equal actualResult expectedResult "the results were different" ]



[<Tests>]
let BinTreeTests =
    testList
        "Tests for BinTree type"
        [
          // AddBinTree
          testPropertyWithConfig config "AddBinTree is commutative"
          <| fun (tree1: CollapsedBinTree<int>) (tree2: CollapsedBinTree<int>) ->

              let res1 = addBinTree tree1.Get tree2.Get addInt
              let res2 = addBinTree tree2.Get tree1.Get addInt

              Expect.equal res1 res2 "the results were different"


          testPropertyWithConfig config "AddBinTree is associative"
          <| fun (tree1: CollapsedBinTree<int>) (tree2: CollapsedBinTree<int>) (tree3: CollapsedBinTree<int>) ->

              let res1 = addBinTree (addBinTree tree1.Get tree2.Get addInt) tree3.Get addInt
              let res2 = addBinTree tree1.Get (addBinTree tree2.Get tree3.Get addInt) addInt

              Expect.equal res1 res2 "the results were different"


          testPropertyWithConfig config "(AddBinTree tree emptyTree) is tree"
          <| fun (tree: CollapsedBinTree<int>) ->

              let emptyTree = BinTree.Empty
              let actualResult = addBinTree tree.Get emptyTree addInt

              Expect.equal actualResult tree.Get "the results were different"


          testPropertyWithConfig config "(AddBinTree tree zeroTree) is tree"
          <| fun (tree: CollapsedBinTreeWithoutEmpty<int>) ->

              let zeroTree = BinTree.Leaf 0
              let actualResult = addBinTree tree.Get zeroTree addInt

              Expect.equal actualResult tree.Get "the results were different"

          // expandBinTree cutBinTree
          testPropertyWithConfig config "tree expansion is the opposite of tree cutting"
          <| fun (vec: Vector<int>) (additionalLen: PositiveInt) ->

              let expectedResult = vec.Data

              let expanded =
                  expandBinTree expectedResult vec.Length (vec.Length + additionalLen.Get)

              let actualResult = cutBinTree expanded vec.Length (vec.Length + additionalLen.Get)

              Expect.equal actualResult expectedResult "the results were different" ]
