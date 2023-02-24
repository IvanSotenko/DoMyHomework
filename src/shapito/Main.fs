namespace DoMyHomework

open System
open System.IO
open System.Collections.Generic

open Matrix
open Vector
open QTree
open BinTree


module randomGenerations =
    let rnd = Random()

    let genRandomArray n =
        Array.init n (fun _ -> Some(rnd.Next(-5, 5)))

    let genRandomNoneArray n =
        Array.init n (fun _ ->
            if rnd.Next(1, 5) = 4 then
                None
            else
                Some(rnd.Next(-10000, 10000)))

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

open naiveConversions
open randomGenerations

module Main =
    [<EntryPoint>]
    let main _ =
        // let path = "../../matrices/lnsp_131.mtx"
        // let path2 = "C:/Users/ivans/Documents/spbsu/DoMyHomework2/matrices/lnsp_131.mtx"
        //
        // printfn $"{path}\n{File.Exists(path)}"

        // let file = File.Create("test.txt")
        // file.Close()

        // let a = AppContext.BaseDirectory
        //
        // printfn $"{a}"

        // let path = "../../../../../matrices/lnsp_131.mtx"
        // printfn $"{FileInfo(path).Extension}"

        // let strs = File.ReadAllLines(path)

        // for i in 0 .. strs.Length - 1 do
        //     printfn $"{strs[i]}"


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




        let readMtxMatrix (path: string) (toType: string -> 'A): Matrix<'A> =

            if FileInfo(path).Extension <> ".mtx" then
                failwith $"Incorrect matrix path: file has wrong extension ({FileInfo(path).Extension})"

            if not (File.Exists(path)) then
                failwith $"Incorrect matrix path: file does not exist ({path})"

            let lines = File.ReadAllLines(path)
            let dict = Dictionary<int * int, 'A>()
            let mutable descriptionOver = false
            let mutable length1, length2 = 0, 0

            for str in lines do
                if str[0] <> '%' then
                    let splittedStr = str.Split(' ')

                    if descriptionOver then
                        dict.Add(((int splittedStr[0], int splittedStr[1]), toType splittedStr[2]))

                    else
                        length1 <- int splittedStr[0]
                        length2 <- int splittedStr[1]
                        descriptionOver <- true

            let initializer (x: int) (y: int) =
                if dict.ContainsKey((x + 1, y + 1)) then
                    Some(dict[(x + 1, y + 1)])
                else
                    None

            Matrix(QTree.init length1 length2 initializer, length1, length2)




        let readMtxMatrixRec (path: string) (toType: string -> 'A): Matrix<'A> =

            if FileInfo(path).Extension <> ".mtx" then
                failwith $"Incorrect matrix path: file has wrong extension ({FileInfo(path).Extension})"

            if not (File.Exists(path)) then
                failwith $"Incorrect matrix path: file does not exist ({path})"

            let lines = File.ReadAllLines(path)
            let len = lines.Length
            let dict = Dictionary<int * int, 'A>()

            let rec dictConstructor length1 length2 descriptionOver i =
                if i + 1 > len then
                    length1, length2
                else
                    let str = lines[i]

                    match str[0] with
                    | '%' -> dictConstructor length1 length2 false (i + 1)

                    | _ when descriptionOver ->
                        let splittedStr = str.Split(' ')
                        dict.Add(((int splittedStr[0], int splittedStr[1]), toType splittedStr[2]))
                        dictConstructor length1 length2 true (i + 1)

                    | _ ->
                        let splittedStr = str.Split(' ')
                        let newLength1, newLength2 = int splittedStr[0], int splittedStr[1]
                        dictConstructor newLength1 newLength2 true (i + 1)


            let initializer (x: int) (y: int) =
                if dict.ContainsKey((x + 1, y + 1)) then
                    Some(dict[(x + 1, y + 1)])
                else
                    None

            let length1, length2 = dictConstructor 0 0 false 0
            Matrix(QTree.init length1 length2 initializer, length1, length2)


        // let a = readMtxMatrix "../../../../../matrices/testMatrix.mtx" float
        // let b = readMtxMatrix2 "../../../../../matrices/testMatrix.mtx" float

        // for i in 1..1000 do
        //     readMtxMatrix "../../../../../matrices/lnsp_131.mtx" float |> ignore
        //     readMtxMatrixRec "../../../../../matrices/lnsp_131.mtx" float |> ignore
        //
        // let start = DateTime.Now
        // for i in 1..1000 do
        //     readMtxMatrixRec "../../../../../matrices/lnsp_131.mtx" float |> ignore
        //     readMtxMatrixRec "../../../../../matrices/football.mtx" float |> ignore
        //     readMtxMatrixRec "../../../../../matrices/cz148.mtx" float |> ignore
        //     readMtxMatrixRec "../../../../../matrices/tub100.mtx" float |> ignore
        // printfn $"Recursive: {(DateTime.Now - start).Milliseconds}"
        //
        //
        // let start = DateTime.Now
        // for i in 1..1000 do
        //     readMtxMatrix "../../../../../matrices/lnsp_131.mtx" float |> ignore
        //     readMtxMatrix "../../../../../matrices/football.mtx" float |> ignore
        //     readMtxMatrix "../../../../../matrices/cz148.mtx" float |> ignore
        //     readMtxMatrix "../../../../../matrices/tub100.mtx" float |> ignore
        // printfn $"Iterative: {(DateTime.Now - start).Milliseconds}"

        let rnd = Random()



        let a = genRandomNoneArray 256
        let vec = Vector(a)
        let tree = vec.Data

        printfn $"{myMinInTree tree}"
        printfn $"{parallelMinInTree 2 tree}"
        0
