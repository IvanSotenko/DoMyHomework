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
type vecMatMultiply () =

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
