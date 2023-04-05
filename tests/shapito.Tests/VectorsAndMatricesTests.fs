module DoMyHomework.Tests.MatrixOperationsTests

open DoMyHomework
open Expecto
open FsCheck
open DoMyHomework.RandomGeneration

open BinTree
open QTree
open Matrix
open Vector
open MatrixAlgebra

let config = { Config.Default with MaxTest = 10000 }

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
        [ testProperty "vecMatMultiply is naiveVecMatMultiply (without None)"
          <| fun (len1: PositiveInt) (len2: PositiveInt) ->

              let mat = genRandomMatrix (int len1) (int len2)
              let vec = genRandomVector (int len1)

              let actualResult = vecMatMultiply vec mat addInt multInt 0
              let expectedResult = naiveVecMatMultiply vec mat addInt multInt

              Expect.equal actualResult.Data expectedResult.Data "the results were different"


          testProperty "vecMatMultiply is naiveVecMatMultiply (with None)"
          <| fun (len1: PositiveInt) (len2: PositiveInt) ->

              let mat = genRandomNoneMatrix (int len1) (int len2)
              let vec = genRandomNoneVector (int len1)

              let actualResult = vecMatMultiply vec mat addInt multInt 0
              let expectedResult = naiveVecMatMultiply vec mat addInt multInt

              Expect.equal actualResult.Data expectedResult.Data "the results were different"


          testCase "Empty vector multiplied by empty matrix is empty vector"
          <| fun _ ->
              let mat = Matrix(Empty, 0, 0)
              let vec = Vector(BinTree.Empty, 0)

              let actualResult = (vecMatMultiply vec mat addInt multInt 0).Data
              Expect.equal actualResult BinTree.Empty "the results were different"


          testProperty "If length of vector dont match with length1 of matrix an exception is thrown"
          <| fun _ ->
              let matLen1, len2 = rnd.Next(2, 100), 1

              // the goal is to make (matLen1 <> vecLen)
              let vecLen =
                  if rnd.Next(1, 3) = 1 then
                      rnd.Next(1, int matLen1)
                  else
                      rnd.Next(int matLen1 + 1, 101)

              let mat = genRandomNoneMatrix (int matLen1) len2
              let vec = genRandomNoneVector vecLen

              Expect.throws
                  (fun _ -> vecMatMultiply vec mat addInt multInt 0 |> ignore)
                  $"The dimensions of the matrix are incompatible
                    for multiplication with the dimensions of the vector:
                    vector length is {vec.Length} but matrix size is {mat.Length1}x{mat.Length2}"


          testProperty "Multiply function returns fully collapsed tree"
          <| fun (len1: PositiveInt) (len2: PositiveInt) ->

              let mat = genRandomNoneMatrix (int len1) (int len2)
              let vec = genRandomNoneVector (int len1)

              let actualResult = (vecMatMultiply vec mat addInt multInt 0).Data
              let expectedResult = collapseBinTree actualResult

              Expect.equal actualResult expectedResult "the results were different" ]


[<Tests>]
let vectorTypeTests =
    testList
        "Tests for Vector type"
        [
          // Index access
          testProperty "Index access test"
          <| fun (len: PositiveInt) ->

              let arr = genRandomNoneArray (int len)
              let vec = Vector(arr)
              let index = rnd.Next(0, int len)

              Expect.equal vec[index] arr[index] "the results were different"


          // Vector constructors
          // Array vector constructor
          testProperty "Array constructor test (arr1 -> vec -> arr2) ==> (arr1 = arr2)"
          <| fun (len: PositiveInt) ->

              let expectedResult = genRandomNoneArray (int len)
              let vec = Vector(expectedResult)
              let actualResult = vectorToArray vec

              Expect.equal actualResult expectedResult "the results were different"


          testProperty "The array Vector constructor returns a fully collapsed tree"
          <| fun (len: PositiveInt) ->

              let actualResult = (genRandomNoneVector (int len)).Data
              let expectedResult = collapseBinTree actualResult

              Expect.equal actualResult expectedResult "the results were different" ]


[<Tests>]
let matrixTypeTests =
    testList
        "Tests for Matrix type"
        [
          // Index access
          testProperty "Index access test"
          <| fun (len1: PositiveInt) (len2: PositiveInt) ->

              let arr2D = genRandomNoneArray2D (int len1) (int len2)
              let mat = Matrix(arr2D)
              let index1, index2 = rnd.Next(0, int len1), rnd.Next(0, int len2)

              Expect.equal mat[index1, index2] arr2D[index1, index2] "the results were different"


          // Matrix constructors
          // Array2D constructor
          testProperty "Array2D constructor tests (arr2D1 -> mat -> arr2D2) ==> (arr2D2 = arr2D2)"
          <| fun (len1: PositiveInt) (len2: PositiveInt) ->

              let expectedResult = genRandomNoneArray2D (int len1) (int len2)
              let mat = Matrix(expectedResult)
              let actualResult = matrixToArray2D mat

              Expect.equal expectedResult actualResult "the results were different"


          testProperty "Array2D Matrix constructor returns a fully collapsed tree"
          <| fun (len1: PositiveInt) (len2: PositiveInt) ->

              let actualResult = (genRandomNoneMatrix (int len1) (int len2)).Data
              let expectedResult = collapseQTree actualResult

              Expect.equal actualResult expectedResult "the results were different" ]



[<Tests>]
let BinTreeTests =
    testList
        "Tests for BinTree type"
        [
          // AddBinTree
          testProperty "AddBinTree is commutative"
          <| fun (len: PositiveInt) ->
              let tree1 = (genRandomNoneVector (int len)).Data
              let tree2 = (genRandomNoneVector (int len)).Data

              let res1 = addBinTree tree1 tree2 addInt
              let res2 = addBinTree tree2 tree1 addInt

              Expect.equal res1 res2 "the results were different"


          testProperty "AddBinTree is associative"
          <| fun (len: PositiveInt) ->
              let tree1 = (genRandomNoneVector (int len)).Data
              let tree2 = (genRandomNoneVector (int len)).Data
              let tree3 = (genRandomNoneVector (int len)).Data

              let res1 = addBinTree (addBinTree tree1 tree2 addInt) tree3 addInt
              let res2 = addBinTree tree1 (addBinTree tree2 tree3 addInt) addInt

              Expect.equal res1 res2 "the results were different"


          testProperty "(AddBinTree tree emptyTree) is tree"
          <| fun (len: PositiveInt) ->
              let tree = (genRandomNoneVector (int len)).Data
              let emptyTree = BinTree.Empty

              let actualResult = addBinTree tree emptyTree addInt

              Expect.equal actualResult tree "the results were different"


          testProperty "(AddBinTree tree zeroTree) is tree"
          <| fun (len: PositiveInt) ->
              let tree = (genRandomVector (int len)).Data

              let zeroTree =
                  Vector(
                      Array.init (int len) (fun _ -> Some 0)
                  )
                      .Data

              let actualResult = addBinTree tree zeroTree addInt

              Expect.equal actualResult tree "the results were different"

          // expandBinTree cutBinTree
          testProperty "tree expansion is the opposite of tree cutting"
          <| fun (len: PositiveInt, additionalLen: PositiveInt) ->
              let expectedResult = (genRandomNoneVector (int len)).Data

              let expanded = expandBinTree expectedResult (int len) (int len + int additionalLen)
              let actualResult = cutBinTree expanded (int len) (int len + int additionalLen)

              Expect.equal actualResult expectedResult "the results were different" ]
