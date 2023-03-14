module DoMyHomework.Tests.BFSTests

open DoMyHomework
open Expecto
open System.Collections.Generic
open DoMyHomework.RandomGeneration

open TestMatrix
open Matrix
open BFS

let vertListToMatrix (verts: list<int * int * 'A>) (length1: int) (length2: int) =
    let toTuple (a, b, c) = ((a, b), c)

    let dict = Dictionary<int * int, 'A>()

    for i in 0 .. verts.Length - 1 do
        dict.Add(toTuple verts[i])

    let initializer (x: int) (y: int) =
        if dict.ContainsKey((x + 1, y + 1)) then
            Some(dict[(x + 1, y + 1)])
        else
            None

    Matrix(QTree.init length1 length2 initializer, length1, length2)

let testMat1 = vertListToMatrix testMatrix1 115 115
let testMat2 = vertListToMatrix testMatrix2 131 131
let testMat3 = vertListToMatrix testMatrix3 100 100


[<Tests>]
let BFSTest =
    testList
        "Tests for BFS.BFS function"
        [ testProperty "Random starting vertices for a single graph (football)"
          <| fun _ ->

              let verts = randomVerts 115u

              let expectedResult = (naiveBFS verts testMat1).Data
              let actualResult = (BFS verts testMat1).Data

              Expect.equal actualResult expectedResult $"the results were different, startVerts = {verts}"


          testProperty "Random starting vertices for a single graph (lnsp_131)"
          <| fun _ ->

              let verts = randomVerts 131u

              let expectedResult = (naiveBFS verts testMat2).Data
              let actualResult = (BFS verts testMat2).Data

              Expect.equal actualResult expectedResult $"the results were different, startVerts = {verts}"


          testProperty "Random starting vertices for a single graph (tub100)"
          <| fun _ ->

              let verts = randomVerts 100u

              let expectedResult = (naiveBFS verts testMat3).Data
              let actualResult = (BFS verts testMat3).Data

              Expect.equal actualResult expectedResult $"the results were different, startVerts = {verts}"


          testCase "With an empty list of starting vertices the result should be all None"
          <| fun _ ->

              let verts = []
              let expectedResult = BinTree.Empty
              let actualResult = (BFS verts testMat3).Data

              Expect.equal actualResult expectedResult "the results were different"


          testProperty "With an empty graph the result should be all None"
          <| fun _ ->

              let verts = randomVerts 100u
              let mat = Matrix(QTree.Empty, 100, 100)

              let expectedResult = BinTree.Empty
              let actualResult = (BFS verts mat).Data

              Expect.equal actualResult expectedResult "the results were different" ]
