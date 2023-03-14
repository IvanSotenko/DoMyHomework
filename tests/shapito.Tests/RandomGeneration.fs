﻿module DoMyHomework.RandomGeneration

open System
open DoMyHomework

open Vector
open Matrix

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

    let initList = [ 1u .. n ]
    let lst = []

    let transfer (listOut: list<uint>) (listIn: list<uint>) =
        let ind = rnd.Next(listOut.Length)
        let el = listOut[ind]
        (List.removeAt ind listOut), (List.append listIn [ el ])

    let rec generator (listDon: uint list) (listRec: uint list) =
        // Setting the possible length of the list here
        if rnd.Next(1, 17) = 1 || listDon.IsEmpty then
            listRec
        else
            let newListDon, newListRec = transfer listDon listRec
            generator newListDon newListRec

    let newInit, newLst = transfer initList lst
    generator newInit newLst
