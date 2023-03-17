module DoMyHomework.Benchmarks

open System
open BenchmarkDotNet.Attributes

open Vector
open Matrix
open MatrixAlgebra


module randomGeneration =
    let rnd = Random()

    let genRandomMatrixWithDensity len1 len2 density =
        if not ((1 <= density) && (density <= 100)) then
            failwith "Incorrect value for density. The density should be in the range from 1 to 100"

        let initializer _ _ =
            if rnd.Next(1, 100) <= density then
                Some(rnd.Next(-1000, 1000))
            else
                None

        let arr2D = Array2D.init len1 len2 initializer

        Matrix(arr2D)

    let genRandomVectorWithDensity len density =
        if not ((1 <= density) && (density <= 100)) then
            failwith "Incorrect value for density. The density should be in the range from 1 to 100"

        let initializer _ =
            if rnd.Next(1, 100) <= density then
                Some(rnd.Next(-1000, 1000))
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
type vectorMap2Benchmark() =

    [<DefaultValue>]
    val mutable vector1: Vector<int>

    [<DefaultValue>]
    val mutable vector2: Vector<int>

    [<Params(100000, 1000000, 5000000)>]
    member val len: int = 0 with get, set

    [<Params(10, 50, 90)>]
    member val density: int = 0 with get, set

    [<GlobalSetup>]
    member self.GlobalSetup() =
        (self.vector1 <- genRandomVectorWithDensity self.len self.density
         self.vector2 <- genRandomVectorWithDensity self.len self.density)

    [<Benchmark>]
    [<Arguments(1)>]
    [<Arguments(2)>]
    [<Arguments(3)>]
    [<Arguments(4)>]
    member self.parallelMap2(pLevel: int) =
        pMap2 addInt self.vector1 self.vector2 pLevel

    [<Benchmark(Baseline = true)>]
    member self.regularMap2() = map2 addInt self.vector1 self.vector2


[<MemoryDiagnoser>]
type vecMatMultiplyBenchmark() =

    [<DefaultValue>]
    val mutable vector: Vector<int>

    [<DefaultValue>]
    val mutable matrix: Matrix<int>

    [<Params(100, 1000, 5000)>]
    member val len1: int = 0 with get, set

    [<Params(100, 1000, 5000)>]
    member val len2: int = 0 with get, set

    [<Params(10, 50, 90)>]
    member val density: int = 0 with get, set


    [<GlobalSetup>]
    member self.GlobalSetup() =
        (self.vector <- genRandomVectorWithDensity self.len1 self.density
         self.matrix <- genRandomMatrixWithDensity self.len1 self.len2 self.density)

    [<Benchmark>]
    [<Arguments(1)>]
    [<Arguments(2)>]
    [<Arguments(3)>]
    [<Arguments(4)>]
    member self.parallelMult(pLevel: int) =
        parallelVecMatMultiply self.vector self.matrix addInt multInt pLevel

    [<Benchmark(Baseline = true)>]
    member self.regularMult() =
        vecMatMultiply self.vector self.matrix addInt multInt
