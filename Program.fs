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
    |> Seq.find(fun (x, y) -> x = input)
    |> snd
    |> string

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
    
let problem15 input =
    let n = Int32.Parse input
    let rec countLattice state step maxStep =
        if step = maxStep
        then
            (state |> Seq.pairwise |> Seq.map(fun (x, y) -> x + y))
            |> Seq.exactlyOne
        else
            countLattice
                (
                    if (step <= maxStep / 2)
                    then
                        (Seq.concat [seq {1L..1L}; (state |> Seq.pairwise |> Seq.map(fun (x, y) -> x + y)); seq {1L..1L}])
                    else
                        (state |> Seq.pairwise |> Seq.map(fun (x, y) -> x + y))
                )
                (step + 1)
                maxStep
    (countLattice (seq{1L..1L}) 1 (n*2))
    |> string

let rec nest p f x = if p=0 then x else nest (p-1) f (f x)
let sumOfDigits = string >> Seq.sumBy (string >> Int32.Parse)

let problem16 input =
    let n = Int32.Parse input
    nest n ((*) 2I) 1I
    |> sumOfDigits
    |> string

let problem20 input =
    let n = Int32.Parse input
    seq {1I .. bigint n}
    |> Seq.fold (fun x y -> bigint.Multiply (x, y)) 1I
    |> sumOfDigits
    |> string

let sumOfProperDivisors n =
    seq {1..(n-1)}
    |> Seq.where(fun y -> n % y = 0)
    |> Seq.sum

let problem21 input =
    let n = Int32.Parse input
    seq {1..n-1}
    |> Seq.where(fun x -> (sumOfProperDivisors >> sumOfProperDivisors) x = x && sumOfProperDivisors x <> x)
    |> Seq.sum
    |> string

let problem23 input =
    let n = Int32.Parse input
    let abundantNumbers = 
        seq {1..n}
        |> Seq.where (fun x -> sumOfProperDivisors x > x)
        |> Seq.toArray
    let canBeWrittenAsTheSumOfTwoAbundantNumbers p = abundantNumbers |> Array.exists(fun x -> Array.contains (p - x) abundantNumbers)
    seq {1..n}
    |> Seq.where(canBeWrittenAsTheSumOfTwoAbundantNumbers >> not)
    |> Seq.sum
    |> string

let problem24 input =
    // We want to obtain the nth permutation.
    let n = Int32.Parse input
    // Define a recursive function to generate a sequence
    let rec getPermutationsWithPDigits (s : string) (p : int) : string seq =
        seq {
            for i in seq {0..9} |> Seq.where (string >> s.Contains >> not) do 
            match p with
            | 1 -> yield (s + (string i))
            | _ -> yield! (getPermutationsWithPDigits (s + (string i)) (p - 1))
        }
    getPermutationsWithPDigits "" 10
    |> Seq.skip (n - 1)
    |> Seq.take 1
    |> Seq.head

let validate (problemNumber : int) (func : string -> string) input expectedOutput =
    "Problem "
    + (String.Concat problemNumber)
    + ". "
    + (
        match (func input) with
        | output when output = expectedOutput -> "PASS (" + output + ")";
        | output -> "FAIL (" + output + "/" + expectedOutput + ")";
    )
    |> Console.WriteLine

do validate 1 problem1 "10" "23"
do validate 1 problem1 "1000" "233168"
do validate 2 problem2 "100" "44"
do validate 2 problem2 "4000000" "4613732"
do validate 3 problem3 "13195" "29"
do validate 3 problem3 "600851475143" "6857"
do validate 4 problem4 "2" "9009"
do validate 4 problem4 "3" "906609"
// TODO: Calculate 5 using prime factors.
// do validate 5 problem5 "10" "2520"
// do validate 5 problem5 "20" "232792560"
do validate 6 problem6 "10" "2640"
do validate 6 problem6 "100" "25164150"
do validate 7 problem7 "6" "13"
do validate 7 problem7 "10001" "104743"
// Skip 8 due to large input.
// TODO: 9.
do validate 10 problem10 "10" "17"
// Skip this because it's too slow.
// do validate 10 problem10 "2000000" "142913828922"
// Skip 11 due to large input.
do validate 12 problem12 "5" "28"
do validate 12 problem12 "500" "76576500"
// Skip 13 due to large input.
// TODO: Validation for 14 is to make sure the sequence from 13 to 1 contains 10 terms.
// Skip 14 because it takes a long time.
// do validate 14 problem14 "1000000" "837799"
do validate 15 problem15 "2" "6"
do validate 15 problem15 "20" "137846528820"
do validate 16 problem16 "15" "26"
do validate 16 problem16 "1000" "1366"
// Skip 17-19 due to large input.
do validate 20 problem20 "10" "27"
do validate 20 problem20 "100" "648"
// No first part for 21.
do validate 21 problem21 "10000" "31626"
// Skip 22 due to large input.
// TODO: Optimise 23.
// do validate 23 problem23 "28123" "0"
// TODO: Optimise
// do validate 24 problem24 "1000000" "2783915460"

