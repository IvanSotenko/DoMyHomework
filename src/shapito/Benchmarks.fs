module DoMyHomework.Benchmarks

open System
open BenchmarkDotNet.Attributes

open Vector
open BinTree
open Matrix
open MatrixAlgebra


module randomGeneration =
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

    let genRandomMatrixWithDensity len1 len2 density =
        if not ((1 <= density) && (density <= 100)) then
            failwith "Incorrect value for density. The density should be in the range from 1 to 100"

        let initializer _ _ =
            if rnd.Next(1, 100) <= density then
                Some (rnd.Next(-1000, 1000))
            else
                None
        let arr2D = Array2D.init len1 len2 initializer

        Matrix(arr2D)

    let genRandomVectorWithDensity len density =
        if not ((1 <= density) && (density <= 100)) then
            failwith "Incorrect value for density. The density should be in the range from 1 to 100"

        let initializer _ =
            if rnd.Next(1, 100) <= density then
                Some (rnd.Next(-1000, 1000))
            else
                None
        let arr = Array.init len initializer

        Vector(arr)


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

open randomGeneration
open OptionIntOperations

[<MemoryDiagnoser>]
type minComparison () =

   [<Params (100, 10000, 1000000)>]
   member val treeSize : int = 0 with get, set

   member self.testTree = (genRandomVector self.treeSize).Data

   [<Benchmark>]
   member self.ParallelMin1() = parallelMinInTree 1 self.testTree

   [<Benchmark>]
   member self.ParallelMin2() = parallelMinInTree 2 self.testTree

   [<Benchmark>]
   member self.ParallelMin3() = parallelMinInTree 3 self.testTree

   [<Benchmark>]
   member self.RegularMin() = myMinInTree self.testTree


[<MemoryDiagnoser>]
type addBinTreeBench () =

   [<Params (1000, 100000, 1000000)>]
   member val treeSize : int = 0 with get, set

   member self.testTree1 = (genRandomVector self.treeSize).Data
   member self.testTree2 = (genRandomVector self.treeSize).Data

   [<Benchmark>]
   member self.AddParallel_2_l1() = parallelAddBinTree self.testTree1 self.testTree2 addInt 1

   [<Benchmark>]
   member self.AddParallel_2_l2() = parallelAddBinTree self.testTree1 self.testTree2 addInt 2

   [<Benchmark>]
   member self.AddParallel_2_l3() = parallelAddBinTree self.testTree1 self.testTree2 addInt 3

   [<Benchmark>]
   member self.RegularAdd() = addBinTree self.testTree1 self.testTree2 addInt


[<MemoryDiagnoser>]
type vecMatMultiplybench () =

   [<Params (10, 100, 1000)>]
   member val len : int = 0 with get, set

   member self.matrix = genRandomNoneMatrix self.len self.len
   member self.vector = genRandomNoneVector self.len

   [<Benchmark>]
   member self.ParallelMult_l1() = parallelVecMatMultiply self.vector self.matrix addInt multInt 1

   [<Benchmark>]
   member self.ParallelMult_l2() = parallelVecMatMultiply self.vector self.matrix addInt multInt 2

   [<Benchmark>]
   member self.ParallelMult_l3() = parallelVecMatMultiply self.vector self.matrix addInt multInt 3

   [<Benchmark>]
   member self.RegularMult() = MatrixAlgebra.vecMatMultiply self.vector self.matrix addInt multInt

   [<Benchmark>]
   member self.naiveMult() = naiveVecMatMultiply self.vector self.matrix addInt multInt


[<MemoryDiagnoser>]
type vecMatMultiplyBenchmark () =

    [<DefaultValue>]
    val mutable vector: Vector<int>
    [<DefaultValue>]
    val mutable matrix: Matrix<int>

    [<Params (100, 1000, 5000)>]
    member val len1 : int = 0 with get, set

    [<Params (100, 1000, 5000)>]
    member val len2 : int = 0 with get, set

    [<Params (10, 50, 90)>]
    member val density : int = 0 with get, set


     [<GlobalSetup>]
     member self.GlobalSetup() = (
         self.vector <- genRandomVectorWithDensity self.len1 self.density
         self.matrix <- genRandomMatrixWithDensity self.len1 self.len2 self.density)

    [<Benchmark>]
    [<Arguments(1)>]
    [<Arguments(2)>]
    [<Arguments(3)>]
    [<Arguments(4)>]
    member self.parallelMult(pLevel: int) = parallelVecMatMultiply self.vector self.matrix addInt multInt

    [<Benchmark>]
    member self.regularMult() = vecMatMultiply self.vector self.matrix addInt multInt


module test =
    let func2 a b =
        for i in 1 .. b do
            for j in 1 .. a do
                i + j |> ignore

    let func1 a =
        for i in 1 .. a do
            i * 2 |> ignore

open test

[<MemoryDiagnoser>]
type testBench () =

    [<Params (5, 7)>]
    member val a : int = 0 with get, set

    [<Benchmark>]
    member self.one() = func1 self.a

    [<Benchmark>]
    [<Arguments(10)>]
    [<Arguments(20)>]
    member self.two(b: int) = func2 self.a b


let alpha (a: int) (b: int) (c: int) (d: string) = printfn $"alpha == a: {a}, b: {b}, c: {c}, d:{d}"
let beta (a: int) (b: int) (c: int) = printfn $"beta == a: {a}, b: {b}, c: {c}"

[<MemoryDiagnoser>]
type testBench2 () =

    [<Params (1, 2)>]
    member val a: int = 0 with get, set

    [<DefaultValue>]
    val mutable b: int

    [<DefaultValue>]
    val mutable c: int

    [<GlobalSetup>]
    member self.Setup() = (
        self.b <- rnd.Next(1,1000)
        self.c <- rnd.Next(1,1000))

    [<Benchmark>]
    [<Arguments("first")>]
    [<Arguments("second")>]
    member self.test1(d: string) = alpha self.a self.b self.c d

    [<Benchmark>]
    member self.test2() = beta self.a self.b self.c


// [<MemoryDiagnoser>]
// type readMtx () =
//
//    [<Benchmark>]
//    member self.iter_cz148() = readMtxMatrix "../../matrices/cz148.mtx" float
//
//    [<Benchmark>]
//    member self.rec_cz148() = readMtxMatrixRec "../../matrices/cz148.mtx" float
//
//    [<Benchmark>]
//    member self.iter_football() = readMtxMatrix "../../matrices/football.mtx" float
//
//    [<Benchmark>]
//    member self.rec_football() = readMtxMatrixRec "../../matrices/football.mtx" float
//
//    [<Benchmark>]
//    member self.iter_lnsp() = readMtxMatrix "../../matrices/lnsp_131.mtx" float
//
//    [<Benchmark>]
//    member self.rec_lnsp() = readMtxMatrixRec "../../matrices/lnsp_131.mtx" float
