namespace DoMyHomework

open System
open System.IO
open System.Collections.Generic
open FSharp.Control
open BenchmarkDotNet.Running

open Matrix
open Vector
open QTree
open BinTree
open Benchmarks
open MatrixAlgebra


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

module mtxReader =
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


module OptionIntOperations =
    let addInt (a: Option<int>) (b: Option<int>) =
        match a, b with
        | Some x, Some y -> Some(x + y)
        | Some x, None -> Some x
        | None, Some x -> Some x
        | None, None -> None

    let multInt (a: Option<int>) (b: Option<int>) =
        match a, b with
        | Some x, Some y -> Some(x * y)
        | _ -> None

open naiveConversions
open randomGenerations
open OptionIntOperations

module Main =
    [<EntryPoint>]
    let main _ =
        // BenchmarkRunner.Run typeof<minComparisonPirate> |> ignore
        // BenchmarkRunner.Run typeof<minComparison> |> ignore
        // BenchmarkRunner.Run typeof<addBinTreeBench> |> ignore
        BenchmarkRunner.Run typeof<vecMatMultiplyBenchmark> |> ignore
        // BenchmarkRunner.Run typeof<testBench2> |> ignore

        0
