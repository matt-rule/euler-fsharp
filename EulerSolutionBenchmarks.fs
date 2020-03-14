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
    
    // // ~ 400ms (Windows)
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

    // // ~ 600ms (Windows)
    // [<Benchmark>]
    // member this.Problem9() =
    //     EulerSolving.problem9 1000

    // [<Benchmark>]
    // member this.Problem10() =
    //     EulerSolving.problem10 10L

    // [<Benchmark>]
    // member this.Problem11() =
    //     EulerSolving.problem11 (File.ReadAllLines "data/problem11.txt") 20

    // // ~ 500ms (Windows)
    // [<Benchmark>]
    // member this.Problem12() =
    //     EulerSolving.problem12 500

    // [<Benchmark>]
    // member this.Problem13() =
    //     EulerSolving.problem13 (File.ReadAllLines "data/problem13.txt")

    // // ~ 700ms (Windows)
    // [<Benchmark>]
    // member this.Problem14() =
    //     EulerSolving.problem14 1000000L

    // [<Benchmark>]
    // member this.Problem15() =
    //     EulerSolving.problem15 20

    // [<Benchmark>]
    // member this.Problem16() =
    //     EulerSolving.problem16 1000

    // [<Benchmark>]
    // member this.Problem17() =
    //     EulerSolving.problem17 1000

    // [<Benchmark>]
    // member this.problem18() =
    //     EulerSolving.problem18 (File.ReadAllLines "data/problem18.txt")

    // [<Benchmark>]
    // member this.Problem19() =
    //     EulerSolving.problem19

    // [<Benchmark>]
    // member this.Problem20() =
    //     EulerSolving.problem20 100

    // [<Benchmark>]
    // member this.Problem21() =
    //     EulerSolving.problem21 10000

    // [<Benchmark>]
    // member this.Problem22() =
    //     EulerSolving.problem22 (File.ReadAllText "data/problem22.txt")

    // [<Benchmark>]
    // member this.Problem23() =
    //     EulerSolving.problem23 28123

    // // ~ 700ms (Windows)
    // [<Benchmark>]
    // member this.Problem24() =
    //     EulerSolving.problem24 1000000

    // [<Benchmark>]
    // member this.Problem25() =
    //     EulerSolving.problem25 1000

    // [<Benchmark>]
    // member this.Problem26() =
    //     EulerSolving.problem26 10

    // // < 1s (Windows)
    // [<Benchmark>]
    // member this.Problem27() =
    //     EulerSolving.problem27 1000

    // [<Benchmark>]
    // member this.Problem28() =
    //     EulerSolving.problem28 500

    // [<Benchmark>]
    // member this.Problem29() =
    //     EulerSolving.problem29 100

    // // ~ 230ms (Windows)
    // [<Benchmark>]
    // member this.Problem30() =
    //     EulerSolving.problem30 5

    // ~ 700ms (Windows)
    // [<Benchmark>]
    // member this.Problem31() =
    //     EulerSolving.problem31 200

    // Optimised to here (Windows)

    // // TODO: Needs optimising! (20.0 secs)
    // [<Benchmark>]
    // member this.Problem32() =
    //     EulerSolving.problem32 ()

    // [<Benchmark>]
    // member this.Problem33() =
    //     EulerSolving.problem33 ()

    // // TODO: Needs optimising! (1.8 secs)
    // [<Benchmark>]
    // member this.Problem34() =
    //     EulerSolving.problem34 1000000

    // // TODO: Needs optimising! (1.5 secs)
    // [<Benchmark>]
    // member this.Problem35() =
    //     EulerSolving.problem35 1000000

    // // TODO: Needs optimising! (2.7 secs)
    // [<Benchmark>]
    // member this.Problem36() =
    //     EulerSolving.problem36 1000000

    // [<Benchmark>]
    // member this.Problem37() =
    //     EulerSolving.problem37 ()

    // [<Benchmark>]
    // member this.Problem38() =
    //     EulerSolving.problem38 ()

    // // TODO: Needs optimising! (1.3 secs)
    // [<Benchmark>]
    // member this.Problem39() =
    //     EulerSolving.problem39 1000

    // [<Benchmark>]
    // member this.Problem40() =
    //     EulerSolving.problem40 6

    // // TODO: Needs optimising! (1.1 secs)
    // [<Benchmark>]
    // member this.Problem41() =
    //     EulerSolving.problem41 ()

    // [<Benchmark>]
    // member this.Problem42() =
    //     EulerSolving.problem42 (File.ReadAllText "data/problem42.txt") 200

    // [<Benchmark>]
    // member this.Problem43() =
    //     EulerSolving.problem43 ()

    // // TODO: Needs optimising! (4.4 secs)
    // [<Benchmark>]
    // member this.Problem44() =
    //     EulerSolving.problem44 100000000L

    // [<Benchmark>]
    // member this.Problem45() =
    //     EulerSolving.problem45 40755L 100000000000L

    // [<Benchmark>]
    // member this.Problem46() =
    //     EulerSolving.problem46 ()

    // TODO: Needs optimising! (13m32s)
    // Optimisation attempt 1: 10m 46s
    // Optimisation attempt 2: ~ 8m
    [<Benchmark>]
    member this.Problem47() =
        EulerSolving.problem47 4

    // [<Benchmark>]
    // member this.Problem48() =
    //     EulerSolving.problem48 1000

    // // TODO: Needs optimising! (4.0 secs)
    // [<Benchmark>]
    // member this.Problem49() =
    //     EulerSolving.problem49 "148748178147"

    // // TODO: Needs optimising! (2.6 secs)
    // [<Benchmark>]
    // member this.Problem50() =
    //     EulerSolving.problem50 1000000
