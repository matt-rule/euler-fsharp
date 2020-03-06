namespace EulerApp

open BenchmarkDotNet.Attributes
open System.IO;

type EulerSolutionBenchmarks() =
    // [<Benchmark>]
    // member this.Problem1() =
    //     EulerSolving.problem1 10

    // [<Benchmark>]
    // member this.Problem2() =
    //     EulerSolving.problem2 4000000
    
    // [<Benchmark>]
    // member this.Problem3() =
    //     EulerSolving.problem3 600851475143L
    
    // [<Benchmark>]
    // member this.Problem4() =
    //     EulerSolving.problem4 3

    // [<Benchmark>]
    // member this.Problem5() =
    //     EulerSolving.problem5 20
    
    // [<Benchmark>]
    // member this.Problem6() =
    //     EulerSolving.problem6 100
    
    // [<Benchmark>]
    // member this.Problem7() =
    //     EulerSolving.problem7 10001

    // [<Benchmark>]
    // member this.Problem8() =
    //     EulerSolving.problem8 (File.ReadAllLines "data/problem8.txt") 13

    // [<Benchmark>]
    // member this.Problem9() =
    //     EulerSolving.problem9 1000

    // [<Benchmark>]
    // member this.Problem10() =
    //     EulerSolving.problem10 10L

    // [<Benchmark>]
    // member this.Problem11() =
    //     EulerSolving.problem11 (File.ReadAllLines "data/problem11.txt") 20

    // [<Benchmark>]
    // member this.Problem12() =
    //     EulerSolving.problem12 500

    // [<Benchmark>]
    // member this.Problem13() =
    //     EulerSolving.problem13 (File.ReadAllLines "data/problem13.txt")

    [<Benchmark>]
    member this.Problem14() =
        EulerSolving.problem14 1000000L

    // [<Benchmark>]
    // member this.Problem15() =
    //     EulerSolving.problem15

    // [<Benchmark>]
    // member this.Problem16() =
    //     EulerSolving.problem16

    // [<Benchmark>]
    // member this.Problem17() =
    //     EulerSolving.problem17