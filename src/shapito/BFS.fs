module DoMyHomework.BFS

open System.Collections.Generic
open MatrixAlgebra
open Vector
open Matrix
open BinTree

// Non-naive recursive BFS using MatrixAlgebra
type Marker = | Mark

let mult a b =
    match a, b with
    | Some x, Some _ -> Some x
    | _ -> None

let add a b =
    match a, b with
    | Some x, _
    | _, Some x -> Some x
    | None, None -> None

let frontMask a b =
    match a, b with
    | Some x, None -> Some x
    | _ -> None

let resultMask iterNum a b =
    match a, b with
    | Some x, _ -> Some x
    | None, Some _ -> Some iterNum
    | None, None -> None

let BFS (startVertices: uint list) (adjMat: Matrix<'A>) =

    let set = Set.ofList startVertices
    let initMarkFront (index: int) =
        if Set.contains (uint (index + 1)) set then
            Some Mark
        else
            None

    let front = Vector(init adjMat.Length1 initMarkFront, adjMat.Length1)
    let result = Vector(Empty, adjMat.Length1)

    let rec subBFS front result iterNum =

        let newFront = map2 frontMask (vecMatMultiply front adjMat add mult) result

        if newFront.IsEmpty then
            result
        else
            subBFS newFront (map2 (resultMask iterNum) result newFront) (iterNum + 1u)

    subBFS front result 1u



// Naive iterative BFS using queue
let increaseBy1 (a: Option<uint>) =
    match a with
    | Some x -> Some(x + 1u)
    | None -> Some 1u

let naiveBFS (startVertices: uint list) (adjMat: Matrix<'A>) =

    let mutable result = Array.create adjMat.Length1 None
    let mutable flag = false
    let queue = Queue(startVertices)
    let startVertsSet = Set.ofList startVertices

    while queue.Count > 0 do
        let v = queue.Dequeue()

        // if there are no more starting vertices in the queue we set flag to true
        if not flag then
            if not (Set.contains v startVertsSet) then
                flag <- true

        // v/w - uint name values, numbered from 1u
        // indV/indW - int index values, numbered from 0
        let indV = int (v - 1u)

        // We need to go through the row of the matrix
        // with index indV to find all vertices adjacent to v
        for indW in 0 .. (adjMat.Length2 - 1) do
            let value = adjMat[indV, indW]

            // if expression (value <> None) is true, we found a vertex adjacent to v
            if value <> None then

                // checking if we have visited this vertex
                if result[indW] = None then

                    // to avoid errors, if the parent of the vertex is the starting vertex, just write 1 to the result
                    if flag then
                        result[indW] <- increaseBy1 result[indV]
                    else
                        result[indW] <- Some 1u

                    queue.Enqueue(uint (indW + 1))

    Vector(result)


let pBFS (startVertices: uint list) (adjMat: Matrix<'A>) pLevel =

    let set = Set.ofList startVertices
    let initMarkFront (index: int) =
        if Set.contains (uint (index + 1)) set then
            Some Mark
        else
            None

    let front = Vector(init adjMat.Length1 initMarkFront, adjMat.Length1)
    let result = Vector(Empty, adjMat.Length1)

    let rec subBFS front result iterNum =

        let newFront = map2 frontMask (parallelVecMatMultiply front adjMat add mult pLevel) result

        if newFront.IsEmpty then
            result
        else
            subBFS newFront (pMap2 (resultMask iterNum) result newFront pLevel) (iterNum + 1u)

    subBFS front result 1u
