module DoMyHomework.BFS
open MatrixAlgebra
open Vector
open Matrix
open BinTree

let add (a: int option) (b: int option) =
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


let createFront (startVerts: List<uint>): Vector<'A> = Vector(Empty, 0)

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
