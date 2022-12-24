module DoMyHomework.BFS
open System.Collections.Generic
open MatrixAlgebra
open Vector
open Matrix
open BinTree

module StupidBFS =

    let add a b =
        match a, b with
        | Some x, Some y -> Some (x + y)
        | _ -> None

    let min a b =
        match a, b with
        | Some x, Some y -> Some (min x y)
        | Some x, None -> Some x
        | None, Some x -> Some x
        | None, None -> None

    let frontMask (a: Option<'A>) (b: Option<'A>) =
        match a, b with
        | Some x, Some y -> if x <= y then Some x else None
        | Some x, None -> Some x
        | _ -> None

    let f' (a: Option<'A>) (b: Option<'A>) =
        match a, b with
        | Some x, Some _ -> Some x
        | Some x, None -> Some x
        | None, Some x -> Some x
        | None, None -> None

    let updateFront (front: Vector<'A>) (result: Vector<'A>) =
        Vector((addBinTree front.Data result.Data frontMask), front.Length)

    let updateResult (front: Vector<'A>) (result: Vector<'A>) =
        Vector((addBinTree front.Data result.Data f'), front.Length)

    let naiveBFS (startVertices: Vector<int>) (mat: Matrix<int>) =
        let mutable result = Vector(Array.create startVertices.Length None)
        let mutable front = startVertices
        let mutable delta = 0
        let mutable flag = false

        while flag = false do
            front <- vecMatMultiply front mat min add
            front <- updateFront front result
            delta <- (foldBinTree (+) 0 result.Data)
            result <- updateResult front result
            flag <- delta = (foldBinTree (+) 0 result.Data)
        result


    let BFS (startVertices: Vector<int>) (mat: Matrix<int>) =
        let result = Vector(Array.create startVertices.Length None)

        let rec subBFS (front: Vector<int>) (result: Vector<int>) =
            let newFront = updateFront (vecMatMultiply front mat min add) result
            let newResult = updateResult newFront result

            if (foldBinTree (+) 0 result.Data) = (foldBinTree (+) 0 newResult.Data) then
                newResult
            else
                subBFS newFront newResult

        subBFS startVertices result

module EnlightenedBFS =
    type Marker =
    | Mark

    let mult a b =
        match a, b with
        | Some x, Some _ -> Some x
        | _ -> None

    let add a b =
        match a, b with
        | Some x, _ -> Some x
        | _, Some x -> Some x
        | None, None -> None

    let frontMask a b =
        match a, b with
        | Some x, None -> Some x
        | _ -> None

    let resultMask iterNum a b =
        match a, b with
        | Some x, Some _ -> Some x
        | Some x, None -> Some x
        | None, Some _ -> Some iterNum
        | None, None -> None

    let updateFront (front: Vector<Marker>) (result: Vector<uint>) =
        Vector((addBinTree front.Data result.Data frontMask), front.Length)

    let updateResult (front: Vector<Marker>) (result: Vector<uint>) iterNum =
        Vector((addBinTree result.Data front.Data (resultMask iterNum)), result.Length)

    let BFS (startVertices: uint list) (adjMat: Matrix<'A>) =

        let result = Vector(Empty, adjMat.Length1)
        let front = Vector(startVertices, adjMat.Length1, Mark)

        let rec subBFS front result iterNum =
            let newFront = updateFront (vecMatMultiply front adjMat add mult) result

            if newFront.isEmpty then
                result
            else
                subBFS newFront (updateResult newFront result iterNum) (iterNum + 1u)

        subBFS front result 1u


    let increaseBy1 (a: Option<uint>) =
        match a with
        | Some x -> Some (x + 1u)
        | None -> Some 1u

    let naiveBFS (startVertices: uint list) (adjMat: Matrix<'A>) =

        let mutable result = Array.create adjMat.Length1 None
        let queue = Queue(startVertices)

        while queue.Count > 0 do
            let v = queue.Dequeue()

            let indV = int (v - 1u)
            for indW in 0 .. (adjMat.Length2 - 1) do
                let value = adjMat[indV, indW]
                if value <> None then

                    if result[indW] = None then
                        result[indW] <- increaseBy1 result[indV]
                        queue.Enqueue(uint (indW + 1))

        Vector(result)
