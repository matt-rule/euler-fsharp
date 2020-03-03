namespace EulerApp

open BenchmarkDotNet.Attributes
open System.IO;

type EulerSolutionBenchmarks() =
    [<Benchmark>]
    member this.Problem1() =
        EulerSolving.problem1 10

    [<Benchmark>]
    member this.Problem2() =
        EulerSolving.problem2 4000000
    
    [<Benchmark>]
    member this.Problem3() =
        EulerSolving.problem3 600851475143L
    
    [<Benchmark>]
    member this.Problem4() =
        EulerSolving.problem4 3

    [<Benchmark>]
    member this.Problem5() =
        EulerSolving.problem5 20
    
    [<Benchmark>]
    member this.Problem6() =
        EulerSolving.problem6 100
    
    [<Benchmark>]
    member this.Problem7() =
        EulerSolving.problem7 10001

    [<Benchmark>]
    member this.Problem8() =
        EulerSolving.problem8 (File.ReadAllLines "data/problem8.txt") 13

    [<Benchmark>]
    member this.Problem9() =
        EulerSolving.problem9 1000