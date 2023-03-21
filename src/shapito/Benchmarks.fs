module DoMyHomework.Benchmarks

open BenchmarkDotNet.Attributes

open Vector
open Matrix
open MatrixAlgebra

open RandomGeneration
open OptionIntOperations

[<MemoryDiagnoser>]
type VectorMap2Benchmark() =

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
    member self.ParallelMap2(pLevel: int) =
        map2 addInt self.vector1 self.vector2 pLevel

    [<Benchmark(Baseline = true)>]
    member self.RegularMap2() = map2 addInt self.vector1 self.vector2 0


[<MemoryDiagnoser>]
type VecMatMultiplyBenchmark() =

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
    member self.ParallelMult(pLevel: int) =
        vecMatMultiply self.vector self.matrix addInt multInt pLevel

    [<Benchmark(Baseline = true)>]
    member self.RegularMult() =
        vecMatMultiply self.vector self.matrix addInt multInt 0
