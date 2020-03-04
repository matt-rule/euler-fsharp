namespace EulerApp

open BenchmarkDotNet.Running
open System;
open System.IO;

module EulerValidation =
    let validate (problemNumber : int) actualOutput expectedOutput =
        "Problem "
        + (string problemNumber)
        + ". "
        +   match expectedOutput with
            | null -> "ATTEMPT (" + actualOutput + ")"
            | expected ->
                match actualOutput with
                | output when output = expected -> "PASS (" + output + ")"
                | output -> "FAIL (" + output + "/" + expected + ")"

    let ValidateOutput() = [|
        validate 1 (EulerSolving.problem1 10) "23";
        validate 1 (EulerSolving.problem1 1000) "233168";
        validate 2 (EulerSolving.problem2 100) "44";
        validate 2 (EulerSolving.problem2 4000000) "4613732";
        validate 3 (EulerSolving.problem3 13195L) "29";
        validate 3 (EulerSolving.problem3 600851475143L) "6857";
        validate 4 (EulerSolving.problem4 2) "9009";
        validate 4 (EulerSolving.problem4 3) "906609";
        validate 5 (EulerSolving.problem5 10) "2520";
        validate 5 (EulerSolving.problem5 20) "232792560";
        validate 6 (EulerSolving.problem6 10) "2640";
        validate 6 (EulerSolving.problem6 100) "25164150";
        validate 7 (EulerSolving.problem7 6) "13";
        validate 7 (EulerSolving.problem7 10001) "104743";
        validate 8 (EulerSolving.problem8 (File.ReadAllLines "data/problem8.txt") 4) "5832";
        validate 8 (EulerSolving.problem8 (File.ReadAllLines "data/problem8.txt") 13) "23514624000";
        // Skip 9 because it takes a long time.
        validate 9 (EulerSolving.problem9 1000) "31875000";
        validate 10 (EulerSolving.problem10 10L) "17";
        // Skip this because it's too slow.
        // validate 10 problem10 "2000000" "142913828922";
        validate 11 (EulerSolving.problem11 (File.ReadAllLines "data/problem11.txt") 20) "70600674";
        validate 12 (EulerSolving.problem12 5) "28";
        validate 12 (EulerSolving.problem12 500) "76576500";
        validate 13 (EulerSolving.problem13 (File.ReadAllLines "data/problem13.txt")) "5537376230";
        // TODO: Validation for 14 is to make sure the sequence from 13 to 1 contains 10 terms.
        // Skip 14 because it takes a long time.
        // validate 14 problem14 "1000000" "837799";
        validate 15 (EulerSolving.problem15 2) "6";
        validate 15 (EulerSolving.problem15 20) "137846528820";
        validate 16 (EulerSolving.problem16 15) "26";
        validate 16 (EulerSolving.problem16 1000) "1366";
        validate 17 (EulerSolving.problem17 5) "19";
        validate 17 (EulerSolving.problem17 1000) "21124";
        validate 18 (EulerSolving.problem18 (File.ReadAllLines "data/problem18.txt")) "1074";
        validate 19 EulerSolving.problem19 "171";
        validate 20 (EulerSolving.problem20 10) "27";
        validate 20 (EulerSolving.problem20 100) "648";
        validate 21 (EulerSolving.problem21 10000) "31626";
        validate 22 (EulerSolving.problem22 (File.ReadAllText "data/problem22.txt")) "871198282";
        // TODO: Optimise 23.
        // validate 23 problem23 "28123" "0";
        // TODO: Optimise
        // validate 24 problem24 "1000000" "2783915460";
        // validate 25 (EulerSolving.problem25 1000) "4782";
        // validate 26 (EulerSolving.problem26 10) "7";
        // validate 26 (EulerSolving.problem26 1000) "983";
        // validate 27 (EulerSolving.problem27 1000) "-59231";
        // validate 28 (EulerSolving.problem28 2) "101";
        // validate 28 (EulerSolving.problem28 500) "669171001";
        // validate 29 (EulerSolving.problem29 5) "15";
        // validate 29 (EulerSolving.problem29 100) "9183";
        // validate 30 (EulerSolving.problem30 4) "19316";
        // validate 30 (EulerSolving.problem30 5) "443839";
        // validate 31 (EulerSolving.problem31 200) "73682";
        // // TODO: Optimise 32.
        // // validate 32 problem32 "45228";
        // validate 33 EulerSolving.problem33 "100";
        // validate 34 (EulerSolving.problem34 1000000) "40730";
        // validate 35 (EulerSolving.problem35 100) "13";
        // validate 35 (EulerSolving.problem35 1000000) "55";
        // validate 36 (EulerSolving.problem36 1000000) "872187";
        // validate 37 EulerSolving.problem37 "748317";
        // validate 38 EulerSolving.problem38 "932718654";
        // validate 39 (EulerSolving.problem39 1000) "840";
        // validate 40 (EulerSolving.problem40 6) "210";
        // validate 41 EulerSolving.problem41 "7652413";
        // validate 42 (EulerSolving.problem42 (File.ReadAllText "data/problem42.txt") 200) "162";

        // // Skip 43 because it's evaluated ahead of time
        // //validate 43 problem43 "16695334890";

        // validate 44 (EulerSolving.problem44 100000000L) "5482660";
        // validate 45 (EulerSolving.problem45 40755L 100000000000L) "1533776805";
        // validate 46 EulerSolving.problem46 "5777";

        // validate 47 (EulerSolving.problem47 2) "14";
        // validate 47 (EulerSolving.problem47 3) "644";
        // // Skip 47 with input 4 because it takes a very long time.
        // //validate 47 (EulerSolving.problem47 4) "134043"";


        // validate 48 (EulerSolving.problem48 10) "0405071317";
        // validate 48 (EulerSolving.problem48 1000) "9110846700";

        // //do validate 49 (EulerSolving.problem49 "148748178147") "296962999629";

        // validate 50 (EulerSolving.problem50 1000000) "997651";
    
    |]

module Program =
    [<EntryPoint>]
    let main args =
        printfn "args: %A" args
        printfn "env.cmdline: %A" <| Environment.GetCommandLineArgs()   

        if args |> Array.length > 1
        then
            printfn "%A" (BenchmarkRunner.Run<EulerSolutionBenchmarks>()) 
            0
        else
            printfn "%A" (EulerValidation.ValidateOutput())
            0
