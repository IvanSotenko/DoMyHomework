module DoMyHomework.Benchmarks

open System
open BenchmarkDotNet.Attributes

open Vector
open BinTree
open pirateBinTree
open Matrix
open MatrixAlgebra

let rnd = Random()

let genArr1 n =
    Array.init n (fun _ ->
        if rnd.Next(1, 5) = 4 then
            None
        else
            Some(rnd.Next(-10000, 10000)))

let genvec n = Vector(genArr1 n)

let genRandomNoneArray2D x y =
    Array2D.init x y (fun _ _ ->
        if rnd.Next(1, 5) = 4 then
            None
        else
            Some(rnd.Next(-5, 5)))

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

open OptionIntOperations

[<MemoryDiagnoser>]
type minComparison () =

   [<Params 100000>]
   member val treeSize :int = 0 with get, set

   member self.testTree = (genvec self.treeSize).Data

   [<Benchmark>]
   member self.ParallelMin1() = parallelMinInTree 1 self.testTree

   [<Benchmark>]
   member self.ParallelMin2() = parallelMinInTree 2 self.testTree

   [<Benchmark>]
   member self.ParallelMin3() = parallelMinInTree 3 self.testTree

   [<Benchmark>]
   member self.RegularMin() = myMinInTree self.testTree


[<MemoryDiagnoser>]
type minComparisonPirate () =

   [<Params (10, 17)>]
   member val treeHeight :int = 0 with get, set

   member self.testTree = generateRandomFullTreeOfHeight rnd self.treeHeight

   [<Benchmark>]
   member self.RegularMin() = minInTree self.testTree

   [<Benchmark>]
   member self.ParallelMin_l1() = parallelMin 1 self.testTree

   [<Benchmark>]
   member self.ParallelMin_l2() = parallelMin 2 self.testTree

   [<Benchmark>]
   member self.ParallelMin2_l1() = parallelMin2 1 self.testTree

   [<Benchmark>]
   member self.ParallelMin2_l2() = parallelMin2 2 self.testTree

[<MemoryDiagnoser>]
type addBinTreeBench () =

   [<Params 1000000>]
   member val treeSize :int = 0 with get, set

   member self.testTree1 = (genvec self.treeSize).Data
   member self.testTree2 = (genvec self.treeSize).Data

   // [<Benchmark>]
   // member self.AddParallel_1_l1() = parallelAddBinTree1 self.testTree1 self.testTree2 addInt 1

   [<Benchmark>]
   member self.AddParallel_1_l4() = parallelAddBinTree1 self.testTree1 self.testTree2 addInt 5

   // [<Benchmark>]
   // member self.AddParallel_2_l2() = parallelAddBinTree2 self.testTree1 self.testTree2 addInt 2

   [<Benchmark>]
   member self.AddParallel_2_l4() = parallelAddBinTree2 self.testTree1 self.testTree2 addInt 5

   // [<Benchmark>]
   // member self.RegularAdd() = addBinTree self.testTree1 self.testTree2 addInt


[<MemoryDiagnoser>]
type vecMatMultiply () =

   [<Params (1000, 10000, 100000)>]
   member val len : int = 0 with get, set

   member self.matrix = genRandomNoneMatrix self.len self.len
   member self.vector = genvec self.len

   [<Benchmark>]
   member self.ParallelMult_l1() = parallelVecMatMultiply self.vector self.matrix addInt multInt 1

   [<Benchmark>]
   member self.ParallelMult_l2() = parallelVecMatMultiply self.vector self.matrix addInt multInt 2

   [<Benchmark>]
   member self.RegularMult() = MatrixAlgebra.vecMatMultiply self.vector self.matrix addInt multInt

   [<Benchmark>]
   member self.naiveMult() = naiveVecMatMultiply self.vector self.matrix addInt multInt
