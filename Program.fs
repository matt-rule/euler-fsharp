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

let isPrime x =
    x > 1 && (
        {2..(float >> sqrt >> int) x}
        |> Seq.forall (fun y -> x % y <> 0)
    )

let isPrime64 l =
    l > 1L && (
        {2L..(float >> sqrt >> int64) l}
        |> Seq.forall (fun y -> l % y <> 0L)
    )

let problem7 n =
    let input = Int32.Parse n
    let naturalNumbers = Seq.initInfinite(fun x -> x + 1)
    naturalNumbers
    |> Seq.where isPrime
    |> Seq.zip naturalNumbers
    |> Seq.where(fun (x, y) -> x = input)
    |> Seq.head
    |> snd
    |> string

let problem8 n = "Not implemented."

let problem9 n = "Not implemented."
    // let input = Int32.Parse n
    // seq { 1..input }
    // |> Seq.map (fun a ->
    //     seq { (a+1)..input }
    //     |> Seq.map (fun b ->
    //         seq { b+1..input }
    //         |> Seq.find (fun c ->
    //             (a*a + b*b = c*c) && (a + b + c = input)
    //         )
    //     )
    // )

let problem10 n =
    let input = Int64.Parse n
    seq {2L..input}
    |> Seq.where isPrime64
    |> Seq.sum
    |> string

let problem11 n = "Not implemented."
    
let problem12 n =
    let input = Int32.Parse n
    // Triangular numbers.
    Seq.unfold(fun (acc, n) -> Some(acc + n, (acc + n, n + 1))) (0, 1)
    |> Seq.find(fun x ->
        // Get all divisors for x up to sqrt(x)
        // This might count the square root twice
        seq {1..x |> (float >> sqrt >> int)}
        |> Seq.where(fun y -> x % y = 0)
        |> Seq.length > input / 2
    )
    |> string

let problem13 n = "Not implemented."
    
let problem14 n =
    let input = Int64.Parse n
    // Collatz sequence.
    let collatzSequence = Seq.unfold(fun x ->
        if x = 1L then None else
        Some(
            let next = (if x % 2L = 0L then (x / 2L) else (x*3L + 1L))
            (next, next)
        )
    )
    seq {1L..input-1L}
    |> Seq.map(fun x -> (x, x |> collatzSequence |> Seq.length))
    |> Seq.fold(fun (x : int64 * int) (y : int64 * int) -> if (snd x) > (snd y) then x else y) (1L, 1)
    |> fst
    |> string

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
        testInput = "";
        testOutput = "";
        problemInput = "";
        problemOutput = "";
        runsSlowly = false
        // testInput = "25";
        // testOutput = "12";
        // problemInput = "1000";
        // problemOutput = "";
        // runsSlowly = false
    };
    {
        func = problem10;
        testInput = "10";
        testOutput = "17";
        problemInput = "2000000";
        problemOutput = "142913828922";
        runsSlowly = false
    };
    {
        func = problem11;
        testInput = "";
        testOutput = "";
        problemInput = "";
        problemOutput = "";
        runsSlowly = false
    };
    {
        func = problem12;
        testInput = "5";
        testOutput = "28";
        problemInput = "500";
        problemOutput = "76576500";
        runsSlowly = false
    };
    {
        func = problem13;
        testInput = "";
        testOutput = "";
        problemInput = "";
        problemOutput = "";
        runsSlowly = false
    };
    {
        func = problem14;
        testInput = "1";
        testOutput = "1";
        problemInput = "1000000";
        problemOutput = "837799";
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
        | s -> "FAIL" + "(" + s.ToString() + "/" + y.problemOutput.ToString() + ")";
    )
)
|> Seq.fold(fun x y -> x + Environment.NewLine + y) ""
|> Console.WriteLine
