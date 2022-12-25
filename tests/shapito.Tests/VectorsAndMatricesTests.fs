module DoMyHomework.Tests.MatrixOperationsTests

open System
open DoMyHomework
open Expecto
open FsCheck

open BinTree
open QTree
open Matrix
open Vector
open MatrixAlgebra
open BFS

let config = { Config.Default with MaxTest = 10000 }

module randomGenerations =
    let rnd = Random()

    let genRandomArray n =
        Array.init n (fun _ -> Some(rnd.Next(-5, 5)))

    let genRandomNoneArray n =
        Array.init n (fun _ ->
            if rnd.Next(1, 5) = 4 then
                None
            else
                Some(rnd.Next(-5, 5)))

    let genRandomVector n = Vector(genRandomArray n)
    let genRandomNoneVector n = Vector(genRandomNoneArray n)

    let genRandomOptionMarkerArray n =
        Array.init n (fun _ ->
            if rnd.Next(1, 5) = 4 then
                None
            else
                Some Mark)

    let genRandomArray2D x y =
        Array2D.init x y (fun _ _ -> Some(rnd.Next(-5, 5)))

    let genRandomNoneArray2D x y =
        Array2D.init x y (fun _ _ ->
            if rnd.Next(1, 5) = 4 then
                None
            else
                Some(rnd.Next(-5, 5)))

    let genRandomMatrix x y = Matrix(genRandomArray2D x y)
    let genRandomNoneMatrix x y = Matrix(genRandomNoneArray2D x y)


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
                    vertList <- List.append vertList [(i + 1, j + 1, unpackOption value)]
        vertList
    let arrayToUintList (arr: Option<'A> []) =
        let mutable uintList = []

        for i in 0 .. arr.Length - 1 do
            if arr[i] <> None then
                uintList <- List.append uintList [uint (i + 1)]
        uintList


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
open naiveConversions
open randomGenerations

[<Tests>]
let multiplyTests =
    testList
        "Tests for Multiply.vecMatMultiply function"
        [ testProperty "vecMatMultiply is naiveVecMatMultiply (without None)"
          <| fun _ ->
              let len1, len2 = rnd.Next(1, 25), rnd.Next(1, 25)

              let mat = genRandomMatrix len1 len2
              let vec = genRandomVector len1

              let actualResult = vecMatMultiply vec mat addInt multInt
              let expectedResult = naiveVecMatMultiply vec mat addInt multInt

              Expect.equal actualResult.Data expectedResult.Data "the results were different"


          testProperty "vecMatMultiply is naiveVecMatMultiply (with None)"
          <| fun _ ->
              let len1, len2 = rnd.Next(1, 25), rnd.Next(1, 25)

              let mat = genRandomNoneMatrix len1 len2
              let vec = genRandomNoneVector len1

              let actualResult = vecMatMultiply vec mat addInt multInt
              let expectedResult = naiveVecMatMultiply vec mat addInt multInt

              Expect.equal actualResult.Data expectedResult.Data "the results were different"


          testCase "Empty vector multiplied by empty matrix is empty vector"
          <| fun _ ->
              let mat = Matrix(Empty, 0, 0)
              let vec = Vector(BinTree.Empty, 0)

              let actualResult = (vecMatMultiply vec mat addInt multInt).Data
              Expect.equal actualResult BinTree.Empty "the results were different"


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
                  (fun _ -> vecMatMultiply vec mat addInt multInt |> ignore)
                  $"The dimensions of the matrix are incompatible
                    for multiplication with the dimensions of the vector:
                    vector length is {vec.Length} but matrix size is {mat.Length1}x{mat.Length2}"


          testProperty "Multiply function returns fully collapsed tree"
          <| fun _ ->
              let len1, len2 = rnd.Next(1, 100), rnd.Next(1, 100)

              let mat = genRandomNoneMatrix len1 len2
              let vec = genRandomNoneVector len1

              let actualResult = (vecMatMultiply vec mat addInt multInt).Data
              let expectedResult = collapseBinTree actualResult

              Expect.equal actualResult expectedResult "the results were different" ]


[<Tests>]
let vectorTypeTests =
    testList
        "Tests for Vector type"
        [
          // Index access
          testProperty "Index access test"
          <| fun _ ->
              let len = rnd.Next(1, 100)

              let arr = genRandomNoneArray len
              let vec = Vector(arr)
              let index = rnd.Next(0, len)

              Expect.equal vec[index] arr[index] "the results were different"


          // Vector constructors
          // Array vector constructor
          testProperty "Array constructor test (arr1 -> vec -> arr2) ==> (arr1 = arr2)"
          <| fun _ ->
              let len = rnd.Next(1, 100)

              let expectedResult = genRandomNoneArray len
              let vec = Vector(expectedResult)
              let actualResult = vectorToArray vec

              Expect.equal actualResult expectedResult "the results were different"


          testProperty "The array Vector constructor returns a fully collapsed tree"
          <| fun _ ->
              let len = rnd.Next(1, 100)

              let actualResult = (genRandomNoneVector len).Data
              let expectedResult = collapseBinTree actualResult

              Expect.equal actualResult expectedResult "the results were different"


          // UintList vector constructor
          testProperty "UintList constructor test (arr1 -> uintList -> vec -> arr2) ==> (arr1 = arr2)"
          <| fun _ ->
              let len = rnd.Next(1, 100)

              let expectedResult = genRandomOptionMarkerArray len
              let uintList = arrayToUintList expectedResult
              let vec = Vector(uintList, len, Mark)
              let actualResult = vectorToArray vec

              Expect.equal actualResult expectedResult "the results were different"


          testProperty "UintList Vector constructor returns a fully collapsed tree"
          <| fun _ ->
              let len = rnd.Next(1, 100)

              let uintList = arrayToUintList (genRandomOptionMarkerArray len)
              let actualResult = Vector(uintList, len, Mark).Data
              let expectedResult = collapseBinTree actualResult

              Expect.equal actualResult expectedResult "the results were different"]


[<Tests>]
let matrixTypeTests =
    testList
        "Tests for Matrix type"
        [
          // Index access
          testProperty "Index access test"
          <| fun _ ->
              let len1, len2 = rnd.Next(1, 100), rnd.Next(1, 100)

              let arr2D = genRandomNoneArray2D len1 len2
              let mat = Matrix(arr2D)
              let index1, index2 = rnd.Next(0, len1), rnd.Next(0, len2)

              Expect.equal mat[index1, index2] arr2D[index1, index2] "the results were different"


          // Matrix constructors
          // Array2D constructor
          testProperty "Array2D constructor tests (arr2D1 -> mat -> arr2D2) ==> (arr2D2 = arr2D2)"
          <| fun _ ->
              let len1, len2 = rnd.Next(1, 100), rnd.Next(1, 100)

              let expectedResult = genRandomNoneArray2D len1 len2
              let mat = Matrix(expectedResult)
              let actualResult = matrixToArray2D mat

              Expect.equal expectedResult actualResult "the results were different"


          testProperty "Array2D Matrix constructor returns a fully collapsed tree"
          <| fun _ ->
              let len1, len2 = rnd.Next(1, 100), rnd.Next(1, 100)

              let actualResult = (genRandomNoneMatrix len1 len2).Data
              let expectedResult = collapseQTree actualResult

              Expect.equal actualResult expectedResult "the results were different"


          // VertList contructor
          testProperty "VertList constructor tests (arr2D1 -> vertList -> mat -> arr2D2) ==> (arr2D2 = arr2D2)"
          <| fun _ ->
              let len1, len2 = rnd.Next(1, 100), rnd.Next(1, 100)

              let expectedResult = genRandomNoneArray2D len1 len2
              let vertList = array2DToVertList expectedResult
              let mat = Matrix(vertList, len1, len2)
              let actualResult = matrixToArray2D mat

              Expect.equal expectedResult actualResult "the results were different"


          testProperty "VertList Matrix constructor returns a fully collapsed tree"
          <| fun _ ->
              let len1, len2 = rnd.Next(1, 100), rnd.Next(1, 100)

              let vertList = array2DToVertList (genRandomNoneArray2D len1 len2)
              let actualResult = Matrix(vertList, len1, len2).Data
              let expectedResult = collapseQTree actualResult

              Expect.equal actualResult expectedResult "the results were different"]



[<Tests>]
let BinTreeTests =
    testList
        "Tests for BinTree type"
        [
          // AddBinTree
          testProperty "AddBinTree is commutative"
          <| fun _ ->
              let len = rnd.Next(1, 50)
              let tree1 = (genRandomNoneVector len).Data
              let tree2 = (genRandomNoneVector len).Data

              let res1 = addBinTree tree1 tree2 addInt
              let res2 = addBinTree tree2 tree1 addInt

              Expect.equal res1 res2 "the results were different"


          testProperty "AddBinTree is associative"
          <| fun _ ->
              let len = rnd.Next(1, 50)
              let tree1 = (genRandomNoneVector len).Data
              let tree2 = (genRandomNoneVector len).Data
              let tree3 = (genRandomNoneVector len).Data

              let res1 = addBinTree (addBinTree tree1 tree2 addInt) tree3 addInt
              let res2 = addBinTree tree1 (addBinTree tree2 tree3 addInt) addInt

              Expect.equal res1 res2 "the results were different"


          testProperty "(AddBinTree tree emptyTree) is tree"
          <| fun _ ->
              let len = rnd.Next(1, 50)
              let tree = (genRandomNoneVector len).Data
              let emptyTree = BinTree.Empty

              let actualResult = addBinTree tree emptyTree addInt

              Expect.equal actualResult tree "the results were different"


          testProperty "(AddBinTree tree zeroTree) is tree"
          <| fun _ ->
              let len = rnd.Next(1, 50)
              let tree = (genRandomVector len).Data
              let zeroTree = Vector(Array.init len (fun _ -> Some 0)).Data

              let actualResult = addBinTree tree zeroTree addInt

              Expect.equal actualResult tree "the results were different"

          // expandBinTree cutBinTree
          testProperty "tree expansion is the opposite of tree cutting"
          <| fun _ ->
              let len = rnd.Next(1, 50)
              let expectedResult = (genRandomNoneVector len).Data

              let additionalLen = rnd.Next(1, 10)
              let expanded = expandBinTree expectedResult len (len + additionalLen)
              let actualResult = cutBinTree expanded len (len + additionalLen)

              Expect.equal actualResult expectedResult "the results were different" ]
