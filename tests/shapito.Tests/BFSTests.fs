module DoMyHomework.Tests.BFSTests

open System
open DoMyHomework
open Expecto
open FsCheck

open testMatrix
open Matrix
open BFS


module randomVertsGeneration =
    let rnd = Random()

    let randomVerts n =

        let initList = [1u .. n]
        let lst = []

        let transfer (listOut: list<uint>) (listIn: list<uint>) =
            let ind = rnd.Next(listOut.Length)
            let el = listOut[ind]

            (List.removeAt ind listOut), (List.append listIn [el])

        let rec generator (listDon: uint list) (listRec: uint list) =
            if rnd.Next(1,7) = 1 || listDon.IsEmpty then
                listRec
            else
                let newListDon, newListRec = transfer listDon listRec
                generator newListDon newListRec

        let newInit, newLst = transfer initList lst
        generator newInit newLst


let testMat = Matrix(bigM, 115, 115)

open randomVertsGeneration

[<Tests>]
let t =
    testList
        "blah"
        [
         testProperty ""
         <| fun _ ->

             let verts = randomVerts 115u
             // let verts = [uint (rnd.Next(1, 115))]

             let expectedResult = (naiveBFS verts testMat).Data
             let actualResult = (BFS verts testMat).Data

             Expect.equal actualResult expectedResult $"The results were different, startVerts = {verts}"]


