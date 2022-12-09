module DoMyHomework.Tests.MatrixOperationsTests

open System
open DoMyHomework
open Expecto
open FsCheck

open BinTree
open QTree
open Matrix
open Vector
open Multiply

let config = { Config.Default with MaxTest = 10000 }

module randomGenerations =
    let rnd = Random()

    let genRandomArray n =
        Array.init n (fun _ -> Some(rnd.Next(-25, 25)))

    let genRandomNoneArray n =
        Array.init n (fun _ ->
            if rnd.Next(1, 5) = 4 then
                None
            else
                Some(rnd.Next(-25, 25)))

    let genRandomVector n = Vector(genRandomArray n)
    let genRandomNoneVector n = Vector(genRandomNoneArray n)


    let genRandomArray2D x y =
        Array2D.init x y (fun _ _ -> Some(rnd.Next(-25, 25)))

    let genRandomNoneArray2D x y =
        Array2D.init x y (fun _ _ ->
            if rnd.Next(1, 5) = 4 then
                None
            else
                Some(rnd.Next(-25, 25)))

    let genRandomMatrix x y = Matrix(genRandomArray2D x y)
    let genRandomNoneMatrix x y = Matrix(genRandomNoneArray2D x y)


let restoreArray (vec: Vector<_>) = Array.init vec.Length (fun i -> vec[i])

let restoreArray2D (mat: Matrix<_>) =
    Array2D.init mat.Length1 mat.Length2 (fun i j -> mat[i, j])


open randomGenerations

[<Tests>]
let multiplyTests =
    testList
        "Tests for Multiply.vecMatMultiply function"
        [ testProperty "vecMatMultiply is naiveVecMatMultiply (without None)"
          <| fun _ ->
              let len1, len2 = rnd.Next(1, 100), rnd.Next(1, 100)

              let mat = genRandomMatrix len1 len2
              let vec = genRandomVector len1

              let actualResult = vecMatMultiply vec mat (+) (*)
              let expectedResult = naiveVecMatMultiply vec mat (+) (*)

              Expect.equal actualResult.Data expectedResult.Data "the results were different"


          testProperty "vecMatMultiply is naiveVecMatMultiply (with None)"
          <| fun _ ->
              let len1, len2 = rnd.Next(1, 100), rnd.Next(1, 100)

              let mat = genRandomNoneMatrix len1 len2
              let vec = genRandomNoneVector len1

              let actualResult = vecMatMultiply vec mat (+) (*)
              let expectedResult = naiveVecMatMultiply vec mat (+) (*)

              Expect.equal actualResult.Data expectedResult.Data "the results were different"


          testCase "Empty vector multiplied by empty matrix is empty vector"
          <| fun _ ->
              let mat = Matrix(Empty, 0, 0)
              let vec = Vector(BinTree.Empty, 0)

              let res = (vecMatMultiply vec mat (+) (*)).Data
              Expect.equal res BinTree.Empty "the results were different"


          testCase "If length of vector dont match with length1 of matrix an exception is thrown"
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
                  (fun _ -> vecMatMultiply vec mat (+) (*) |> ignore)
                  $"The dimensions of the matrix are incompatible
                    for multiplication with the dimensions of the vector:
                    vector length is {vec.Length} but matrix size is {mat.Length1}x{mat.Length2}" ]


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


          // Array-based vector constructor
          testProperty "Constructor test (arr1 -> vec -> arr2) ==> (arr1 = arr2)"
          <| fun _ ->
              let len = rnd.Next(1, 100)

              let arr1 = genRandomNoneArray len
              let vec = Vector(arr1)
              let arr2 = restoreArray vec

              Expect.equal arr1 arr2 "the results were different"


          // constructBinTree
          testProperty "Constructor test (arr1 -> tree -> vec -> arr2) ==> (arr = arr2) (non-collapsed tree)"
          <| fun _ ->
              let len = rnd.Next(1, 100)

              let arr1 = genRandomNoneArray len
              let tree = constructBinTree arr1
              let vec = Vector(tree, len)
              let arr2 = restoreArray vec

              Expect.equal arr1 arr2 "the results were different" ]


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


          // Array2D-based Matrix constructor
          testProperty "Constructor tests (arr2D1 -> mat -> arr2D2) ==> (arr2D2 = arr2D2)"
          <| fun _ ->
              let len1, len2 = rnd.Next(1, 100), rnd.Next(1, 100)

              let arr2D1 = genRandomNoneArray2D len1 len2
              let mat = Matrix(arr2D1)
              let arr2D2 = restoreArray2D mat

              Expect.equal arr2D1 arr2D2 "the results were different"


          // constructQTree
          testProperty "Constructor test (arrD1 -> tree -> mat -> arrD2) ==> (arr2D1 = arr2D2) (non-collapsed tree)"
          <| fun _ ->
              let len1, len2 = rnd.Next(1, 100), rnd.Next(1, 100)

              let arr2D1 = genRandomNoneArray2D len1 len2
              let tree = constructQTree arr2D1
              let mat = Matrix(tree, len1, len2)
              let arr2D2 = restoreArray2D mat

              Expect.equal arr2D1 arr2D2 "the results were different"

          ]


[<Tests>]
let BinTreeTests =
    testList
        "Tests for BinTree type"
        [
          // AddBinTree
          testProperty "AddBinTree is commutative"
          <| fun _ ->
              let len = rnd.Next(1, 100)
              let tree1 = (genRandomNoneVector len).Data
              let tree2 = (genRandomNoneVector len).Data

              let res1 = addBinTree tree1 tree2 (+)
              let res2 = addBinTree tree2 tree1 (+)

              Expect.equal res1 res2 "the results were different"


          testProperty "AddBinTree is associative"
          <| fun _ ->
              let len = rnd.Next(1, 100)
              let tree1 = (genRandomNoneVector len).Data
              let tree2 = (genRandomNoneVector len).Data
              let tree3 = (genRandomNoneVector len).Data

              let res1 = addBinTree (addBinTree tree1 tree2 (+)) tree3 (+)
              let res2 = addBinTree tree1 (addBinTree tree2 tree3 (+)) (+)

              Expect.equal res1 res2 "the results were different"


          testProperty "(AddBinTree tree emptyTree) is tree"
          <| fun _ ->
              let len = rnd.Next(1, 100)
              let tree = (genRandomNoneVector len).Data
              let emptyTree = BinTree.Empty

              let res = addBinTree tree emptyTree (+)

              Expect.equal res tree "the results were different"


          testProperty "(AddBinTree tree zeroTree) is tree"
          <| fun _ ->
              let len = rnd.Next(1, 100)
              let tree = (genRandomVector len).Data
              let zeroTree = Vector(Array.init len (fun _ -> Some 0)).Data

              let res = addBinTree tree zeroTree (+)

              Expect.equal res tree "the results were different"


          // collapseBinTree
          testProperty "Collapsing does not change the information inside the tree"
          <| fun _ ->
              let len = rnd.Next(1, 100)
              let tree = (genRandomNoneVector len).Data
              let collapsedTree = collapseBinTree tree

              let vec1 = Vector(tree, len)
              let vec2 = Vector(collapsedTree, len)

              let arr1 = restoreArray vec1
              let arr2 = restoreArray vec2

              Expect.equal arr1 arr2 "the results were different"


          // expandBinTree cutBinTree
          testProperty "tree expansion is the opposite of tree cutting"
          <| fun _ ->
              let len = rnd.Next(1, 100)
              let tree = (genRandomNoneVector len).Data

              let additionalLen = rnd.Next(1, 10)
              let expanded = expandBinTree tree len (len + additionalLen)
              let cut = cutBinTree expanded len (len + additionalLen)

              Expect.equal tree cut "the results were different" ]


let QTreeTests =
    testList
        "Tests for QTree type"
        [
          // collapseQTree
          testProperty "Collapsing does not change the information inside the tree"
          <| fun _ ->
              let len1, len2 = rnd.Next(1, 100), rnd.Next(1, 100)
              let tree = (genRandomNoneMatrix len1 len2).Data
              let collapsedTree = collapseQTree tree

              let mat1 = Matrix(tree, len1, len2)
              let mat2 = Matrix(collapsedTree, len1, len2)

              let arr2D1 = restoreArray2D mat1
              let arr2D2 = restoreArray2D mat2

              Expect.equal arr2D1 arr2D2 "the results were different" ]
