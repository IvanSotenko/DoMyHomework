module DoMyHomework.Tests.Generators

open System
open DoMyHomework
open DoMyHomework.Matrix
open FsCheck
open Expecto

open Vector
open BinTree


type MultipliableVectorAndMatrix<'A when 'A: equality> =
    val Matrix: Matrix<'A>
    val Vector: Vector<'A>

    new(vector, matrix) = { Vector = vector; Matrix = matrix }


type UnmatchedVectorAndMatrix<'A when 'A: equality> =
    val Matrix: Matrix<'A>
    val Vector: Vector<'A>

    new(vector, matrix) = { Vector = vector; Matrix = matrix }


type NoneEmptyArray2D<'A> =
    | NoneEmptyArray2D of 'A [,]
    member xs.Get =
        match xs with
        | NoneEmptyArray2D a -> a


type CollapsedBinTree<'A> =
    | CollapsedBinTree of BinTree<'A>
    member x.Get =
        match x with
        | CollapsedBinTree tree -> tree


type CollapsedBinTreeWithoutEmpty<'A> =
    | CollapsedBinTreeWithoutEmpty of BinTree<'A>
    member x.Get =
        match x with
        | CollapsedBinTreeWithoutEmpty tree -> tree


let rnd = Random()


type AlgebraTypes =

    static member VecAndMat() =
        let genFilling =
            Gen.frequency [ (3, Gen.map Some Arb.generate<'A>)
                            (1, Gen.constant None) ]

        let vec s =
            genFilling
            |> Gen.arrayOfLength s
            |> Gen.map Vector

        let mat s =
            genFilling
            |> Gen.array2DOfDim (s, rnd.Next(1, s + s / 2))
            |> Gen.map Matrix

        let vecAndMat s =
            (Gen.zip (vec s) (mat s))
            |> Gen.map MultipliableVectorAndMatrix

        Gen.sized vecAndMat
        |> Gen.scaleSize (fun s -> s * 2 |> float |> sqrt |> int)
        |> Arb.fromGen


    static member NonEmptyArray2D() =
        Arb.generate<'A>
        |> Gen.array2DOf
        |> Gen.filter (fun xs -> xs.Length <> 0)
        |> Gen.map NoneEmptyArray2D
        |> Arb.fromGen


    static member BadVecAndMat() =
        let vec =
            Gen.arrayOf Arb.generate<Option<'A>>
            |> Gen.map Vector

        let mat =
            Gen.array2DOf Arb.generate<Option<'A>>
            |> Gen.map Matrix

        Gen.zip vec mat
        |> Gen.map UnmatchedVectorAndMatrix
        |> Gen.filter (fun vm -> vm.Vector.Length <> vm.Matrix.Length1)
        |> Arb.fromGen


    static member CollapsedBinTree() =
        let rec tree s =
            match s with
            | 0 ->
                Gen.frequency [ (2, Gen.map Leaf Arb.generate<'A>)
                                (1, gen { return Empty }) ]
            | n when n > 0 ->
                let subtrees = Gen.two (tree (n / 2))

                Gen.frequency [ (1, Gen.map Leaf Arb.generate<'A>)
                                (3,
                                 subtrees
                                 |> Gen.filter (fun (x, y) -> x <> y)
                                 |> Gen.map Node) ]
            | _ -> invalidArg "s" "Only positive arguments are allowed"

        Gen.sized tree
        |> Gen.map CollapsedBinTree
        |> Arb.fromGen


    static member CollapsedBinTreeWithoutEmpty() =
        let rec tree s =
            match s with
            | 0 -> Gen.map Leaf Arb.generate<'A>
            | n when n > 0 ->
                let subtrees = Gen.two (tree (n / 2))

                Gen.frequency [ (1, Gen.map Leaf Arb.generate<'A>)
                                (3,
                                 subtrees
                                 |> Gen.filter (fun (x, y) -> x <> y)
                                 |> Gen.map Node) ]
            | _ -> invalidArg "s" "Only positive arguments are allowed"

        Gen.sized tree
        |> Gen.map CollapsedBinTreeWithoutEmpty
        |> Arb.fromGen


    static member Vector() =
        Gen.arrayOf Arb.generate<Option<'A>>
        |> Gen.map Vector
        |> Arb.fromGen


let config =
    { FsCheckConfig.defaultConfig with
        arbitrary = [ typeof<AlgebraTypes> ]
        maxTest = 10000 }
