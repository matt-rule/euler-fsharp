open System

let problem1 n =
    let input = Int32.Parse n
    seq { 1..input-1 }
    |> Seq.where(fun x -> x % 3 = 0 || x % 5 = 0)
    |> Seq.sum
    |> string

let problem2 n =
    let input = Int32.Parse n
    Seq.unfold(fun (x, y) -> Some(x + y, (y, x + y))) (0, 1)
    |> Seq.takeWhile(fun x -> x <= input)
    |> Seq.where(fun x -> x % 2 = 0)
    |> Seq.sum
    |> string

// Check whether this handles 12 correctly because 2 is a prime factor of it twice (2 squared = 4).
let problem3 n =
    let input = Int64.Parse n
    input
    |> Seq.unfold(fun x ->
        if x = 1L then None else
            let y = seq { 2L..x } |> Seq.find(fun y -> x % y = 0L)
            Some(y, x / y)
    )
    |> Seq.max
    |> string


let problem4 n =
    let input = Int32.Parse n
    let max =
        Seq.replicate input 9
        |> String.Concat
        |> Int32.Parse
    seq { 1..max }
    |> Seq.collect(fun x ->
        seq { x+1..max }
        |> Seq.map (fun y -> x*y |> string)
        |> Seq.where (fun y -> y = (y |> seq |> Seq.rev |> String.Concat))
        |> Seq.map(Int32.Parse)
    )
    |> Seq.max
    |> string

// Takes a while. Try to optimise later. For example, could try to convert to string only once we have a value in a variable.
// Though it would be surprising if it didn't do this already.
let problem5 n =
    let input = Int32.Parse n
    Seq.initInfinite(fun x -> x + 1)
    |> Seq.where (fun x ->
        seq {1..input}
        |> Seq.forall(fun y -> x % y = 0)
    )
    |> Seq.head
    |> string

let problem6 n =
    let input = Int32.Parse n
    let naturalNumbers =
        Seq.initInfinite(fun x -> x + 1)
        |> Seq.take input
    let sumOfSquares = Seq.sumBy (fun x -> x*x) naturalNumbers
    let squareOfSum = pown (Seq.sum naturalNumbers) 2
    abs(sumOfSquares - squareOfSum)
    |> string

let problem7 n =
    let input = Int32.Parse n
    let naturalNumbers = Seq.initInfinite(fun x -> x + 1)
    let isPrime x =
        x > 1 && (
            {2..(float >> sqrt >> int) x}
            |> Seq.forall (fun y -> x % y <> 0)
        )
    naturalNumbers
    |> Seq.where isPrime
    |> Seq.zip naturalNumbers
    |> Seq.where(fun (x, y) -> x = input)
    |> Seq.head
    |> snd
    |> string

let problem8 n = "Not implemented."

let problem9 n =
    let input = Int32.Parse n
    seq { 1..input }
    |> (fun x -> (

    ))

type Problem = {
    func : string -> string;
    testInput : string;
    testOutput : string;
    problemInput : string;
    // null if unknown.
    problemOutput : string;
    runsSlowly : bool;
}

let problems = [|
    {
        func = problem1;
        testInput = "10";
        testOutput = "23";
        problemInput = "1000";
        problemOutput = "233168";
        runsSlowly = false
    };
    {
        func = problem2;
        testInput = "100";
        testOutput = "44";
        problemInput = "4000000";
        problemOutput = "4613732";
        runsSlowly = false
    };
    {
        func = problem3;
        testInput = "13195";
        testOutput = "29";
        problemInput = "600851475143";
        problemOutput = "6857";
        runsSlowly = false
    };
    {
        func = problem4;
        testInput = "2";
        testOutput = "9009";
        problemInput = "3";
        problemOutput = "906609";
        runsSlowly = false
    };
    {
        func = problem5;
        testInput = "10";
        testOutput = "2520";
        problemInput = "20";
        problemOutput = "232792560";
        runsSlowly = true
    };
    {
        func = problem6;
        testInput = "10";
        testOutput = "2640";
        problemInput = "100";
        problemOutput = "25164150";
        runsSlowly = false
    };
    {
        func = problem7;
        testInput = "6";
        testOutput = "13";
        problemInput = "10001";
        problemOutput = "104743";
        runsSlowly = false
    };
    {
        func = problem8;
        testInput = "";
        testOutput = "";
        problemInput = "";
        problemOutput = "";
        runsSlowly = false
    };
    {
        func = problem9;
        testInput = "25";
        testOutput = "12";
        problemInput = "1000";
        problemOutput = "";
        runsSlowly = false
    };
|]

// Seq.initInfinite(fun x -> x + 1)
// |> Seq.map(fun x -> (x, isPrime x))
// |> Seq.take 10
// |> Seq.map (fun (x, y) -> x.ToString() + " " + y.ToString())
// |> Seq.fold(fun x y -> x + Environment.NewLine + y) ""
// |> Console.WriteLine

problems
|> Seq.zip(Seq.initInfinite(fun x -> x + 1))
|> Seq.where(fun (x, y) -> not y.runsSlowly)
|> Seq.map(fun (x, y) ->
    "Problem "
    + (String.Concat x)
    + ". "
    + (
        match (y.func y.testInput) with
        | s when s = y.testOutput -> "PASS"
        | "Not implemented." -> "SKIP"
        | _ -> "FAIL"
    )
    + "/"
    + (
        match (y.func y.problemInput) with
        | s when s = y.problemOutput -> "PASS"
        | "Not implemented." -> "SKIP"
        | _ -> "FAIL"
    )
)
|> Seq.fold(fun x y -> x + Environment.NewLine + y) ""
|> Console.WriteLine
