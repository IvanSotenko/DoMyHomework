module DoMyHomework.RandomGeneration

open System

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


let genRandomMatrixWithDensity len1 len2 density =
    if not ((1 <= density) && (density <= 100)) then
        failwith $"Incorrect value for matrix density ({density}). The density should be in the range from 1 to 100"

    let initializer _ _ =
        if rnd.Next(1, 100) <= density then
            Some(rnd.Next(-1000, 1000))
        else
            None

    let arr2D = Array2D.init len1 len2 initializer

    Matrix(arr2D)


let genRandomVectorWithDensity len density =
    if not ((1 <= density) && (density <= 100)) then
        failwith $"Incorrect value for vector density ({density}). The density should be in the range from 1 to 100"

    let initializer _ =
        if rnd.Next(1, 100) <= density then
            Some(rnd.Next(-1000, 1000))
        else
            None

    let arr = Array.init len initializer

    Vector(arr)


let genRandomLength () = rnd.Next(1, 50)
