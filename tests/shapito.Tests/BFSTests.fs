module DoMyHomework.Tests.BFSTests

open System
open DoMyHomework
open Expecto
open FsCheck

open testMatrix
open Matrix
open BFS
open Vector


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

    let randomVerts n =

        let initList = [1u .. n]
        let lst = []

        let transfer (listOut: list<uint>) (listIn: list<uint>) =
            let ind = rnd.Next(listOut.Length)
            let el = listOut[ind]
            (List.removeAt ind listOut), (List.append listIn [el])

        let rec generator (listDon: uint list) (listRec: uint list) =
            // Setting the possible length of the list here
            if rnd.Next(1,17) = 1 || listDon.IsEmpty then
                listRec
            else
                let newListDon, newListRec = transfer listDon listRec
                generator newListDon newListRec

        let newInit, newLst = transfer initList lst
        generator newInit newLst

let testMat1 = Matrix(testMatrix1, 115, 115)
let testMat2 = Matrix(testMatrix2, 132, 132)
let testMat3 = Matrix(testMatrix3, 100, 100)

open randomGenerations

[<Tests>]
let BFSTest =
    testList
        "Tests for BFS.BFS function"
        [
         testProperty "Random starting vertices for a single graph (football)"
         <| fun _ ->

             let verts = randomVerts 115u

             let expectedResult = (naiveBFS verts testMat1).Data
             let actualResult = (BFS verts testMat1).Data

             Expect.equal actualResult expectedResult $"the results were different, startVerts = {verts}"

         testProperty "Random starting vertices for a single graph (lnsp_131)"
         <| fun _ ->

             let verts = randomVerts 132u

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

             Expect.equal actualResult expectedResult "the results were different"
             ]
