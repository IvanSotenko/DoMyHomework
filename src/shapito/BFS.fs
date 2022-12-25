module DoMyHomework.BFS
open System.Collections.Generic
open MatrixAlgebra
open Vector
open Matrix
open BinTree

// Non-naive recursive BFS using MatrixAlgebra
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

        // we multiply the vector by the matrix and apply a "mask" to it
        let newFront = updateFront (vecMatMultiply front adjMat add mult) result

        if newFront.isEmpty then
            result
        else
            // update the result and go to the next iteration
            subBFS newFront (updateResult newFront result iterNum) (iterNum + 1u)

    subBFS front result 1u



// Naive iterative BFS using queue
let increaseBy1 (a: Option<uint>) =
    match a with
    | Some x -> Some (x + 1u)
    | None -> Some 1u

let naiveBFS (startVertices: uint list) (adjMat: Matrix<'A>) =

    let mutable result = Array.create adjMat.Length1 None
    let queue = Queue(startVertices)

    // printfn "%A" (queue.ToArray())

    while queue.Count > 0 do
        let v = queue.Dequeue()

        // v/w - uint name values, numbered from 1u
        // indV/indW - int index values, numbered from 0
        let indV = int (v - 1u)

        // We need to go through the row of the matrix
        // with index indV to find all vertices adjacent to v
        for indW in 0 .. (adjMat.Length2 - 1) do
            let value = adjMat[indV, indW]

            // if expression (value <> None) is true, we found a vertex adjacent to v
            if value <> None then

                if result[indW] = None then

                    if List.contains v startVertices then
                        result[indW] <- Some 1u
                    else
                        result[indW] <- increaseBy1 result[indV]

                    queue.Enqueue(uint (indW + 1))

    // result <- Array.map (fun x -> if x = Some 0u then Some 1u else x) result
    Vector(result)
