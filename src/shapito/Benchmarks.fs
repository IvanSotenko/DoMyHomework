module DoMyHomework.Benchmarks

open System
open BenchmarkDotNet.Attributes

open Vector
open BinTree
open pirateBinTree


let rnd = Random()

let genArr1 n =
    Array.init n (fun _ ->
        if rnd.Next(1, 5) = 4 then
            None
        else
            Some(rnd.Next(-10000, 10000)))

let genvec n = Vector(genArr1 n)


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


let add opt1 opt2 =
    match opt1, opt2 with
    | Some v1, Some v2 -> Some (v1 + v2)
    | Some v1, None -> Some v1
    | None, Some v2 -> Some v2
    | None, None -> None


[<MemoryDiagnoser>]
type addBinTreeBench () =

   [<Params 1000000>]
   member val treeSize :int = 0 with get, set

   member self.testTree1 = (genvec self.treeSize).Data
   member self.testTree2 = (genvec self.treeSize).Data

   // [<Benchmark>]
   // member self.AddParallel_1_l1() = parallelAddBinTree1 self.testTree1 self.testTree2 add 1

   [<Benchmark>]
   member self.AddParallel_1_l4() = parallelAddBinTree1 self.testTree1 self.testTree2 add 5

   // [<Benchmark>]
   // member self.AddParallel_2_l2() = parallelAddBinTree2 self.testTree1 self.testTree2 add 2

   [<Benchmark>]
   member self.AddParallel_2_l4() = parallelAddBinTree2 self.testTree1 self.testTree2 add 5

   // [<Benchmark>]
   // member self.RegularAdd() = addBinTree self.testTree1 self.testTree2 add
