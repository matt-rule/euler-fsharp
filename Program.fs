let problem1 n =
    {1..n-1}
    |> Seq.where(fun x -> x % 3 = 0 || x % 5 = 0)
    |> Seq.sum
    |> string

let problem2 n =
    Seq.unfold(fun (x, y) -> Some(x + y, (y, x + y))) (0, 1)
    |> Seq.takeWhile ((<=) n)
    |> Seq.where(fun x -> x % 2 = 0)
    |> Seq.sum
    |> string

// Check whether this handles 12 correctly because 2 is a prime factor of it twice (2 squared = 4).
let problem3 n =
    n
    |> Seq.unfold(fun x ->
        if x = 1L then None else
            let y = {2L..x} |> Seq.find(fun y -> x % y = 0L)
            Some(y, x / y)
    )
    |> Seq.max
    |> string

let stringConcatFromCharSeq (input : char seq) =
    input |> Seq.map string |> Seq.reduce (+)

let stringConcatFromIntSeq (input : int seq) =
    input |> Seq.map string |> Seq.reduce (+)

let problem4 n =
    let max =
        Seq.replicate n 9
        |> stringConcatFromIntSeq
        |> int
    {1..max}
    |> Seq.collect(fun x ->
        {x+1..max}
        |> Seq.map (fun y -> x*y |> string)
        |> Seq.where (fun y -> y = (y |> seq |> Seq.rev |> stringConcatFromCharSeq))
        |> Seq.map int
    )
    |> Seq.max
    |> string

// Takes a while. Try to optimise later. For example, could try to convert to string only once we have a value in a variable.
// Though it would be surprising if it didn't do this already.
let problem5 n =
    Seq.initInfinite((+) 1)
    |> Seq.where (fun x ->
        {1..n}
        |> Seq.forall(fun y -> x % y = 0)
    )
    |> Seq.head
    |> string

let problem6 n =
    let naturalNumbers =
        Seq.initInfinite((+) 1)
        |> Seq.take n
    let sumOfSquares = Seq.sumBy ((fun x -> x*x)) naturalNumbers
    let squareOfSum = pown (Seq.sum naturalNumbers) 2
    abs(sumOfSquares - squareOfSum)
    |> string

let isPrime x =
    x > 1 && (
        {2..(float >> sqrt >> int) x}
        |> Seq.forall (fun y -> x % y <> 0)         // Consider (fun y -> x % y <> LanguagePrimitives.GenericZero)
    )

let isPrime64 l =
    l > 1L && (
        {2L..(float >> sqrt >> int64) l}
        |> Seq.forall (fun y -> l % y <> 0L)        // Consider (fun y -> x % y <> LanguagePrimitives.GenericZero)
    )

let primes = Seq.initInfinite((+) 1) |> Seq.where isPrime

let problem7 n =
    let naturalNumbers = Seq.initInfinite((+) 1)
    naturalNumbers
    |> Seq.where isPrime
    |> Seq.zip naturalNumbers
    |> Seq.find(fun (x, y) -> x = n)
    |> snd
    |> string

let problem9 n =
    seq {
        for x in 1..n  do
        for y in x+1..n do
        for z in y+1..n do
        if (x*x + y*y = z*z) && (x + y + z = 1000)
        then yield (x * y * z)
    }
    |> Seq.head
    |> string

let problem10 n =
    {2L..n}
    |> Seq.where isPrime64
    |> Seq.sum
    |> string

let intSqrt = float >> sqrt >> int

let problem12 n =
    // Triangular numbers.
    Seq.unfold(fun (acc, n) -> Some(acc + n, (acc + n, n + 1))) (0, 1)
    |> Seq.find(fun x ->
        // Get all divisors for x up to sqrt(x)
        // This might count the square root twice
        {1..(intSqrt x)}
        |> Seq.where(fun y -> x % y = 0)
        |> Seq.length > n / 2
    )
    |> string
    
let problem14 n =
    // Collatz sequence.
    let collatzSequence = Seq.unfold(fun x ->
        if x = 1L then None else
        Some(
            let next = (if x % 2L = 0L then (x / 2L) else (x*3L + 1L))
            (next, next)
        )
    )
    {1L..n-1L}
    |> Seq.map(fun x -> (x, x |> collatzSequence |> Seq.length))
    |> Seq.fold(fun x y -> if (snd x) > (snd y) then x else y) (1L, 1)
    |> fst
    |> string
    
let problem15 n =
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
                        (Seq.concat [{1L..1L}; (state |> Seq.pairwise |> Seq.map(fun (x, y) -> x + y)); {1L..1L}])
                    else
                        (state |> Seq.pairwise |> Seq.map(fun (x, y) -> x + y))
                )
                (step + 1)
                maxStep
    (countLattice {1L..1L} 1 (n*2))
    |> string

let rec nest p f x = if p=0 then x else nest (p-1) f (f x)
let sumOfDigits = string >> Seq.sumBy (string >> int)

let problem16 n =
    nest n ((*) 2I) 1I
    |> sumOfDigits
    |> string

let problem20 (n : int32) =
    {1I..(bigint n)}
    |> Seq.fold (fun x y -> bigint.Multiply (x, y)) 1I
    |> sumOfDigits
    |> string

let sumOfProperDivisors n =
    {1..n-1}
    |> Seq.where(fun y -> n % y = 0)
    |> Seq.sum

let problem21 n =
    {1..n-1}
    |> Seq.where(fun x -> (sumOfProperDivisors >> sumOfProperDivisors) x = x && sumOfProperDivisors x <> x)
    |> Seq.sum
    |> string

let problem23 n =
    let abundantNumbers = 
        {1..n}
        |> Seq.where (fun x -> sumOfProperDivisors x > x)
        |> Seq.toArray
    let canBeWrittenAsTheSumOfTwoAbundantNumbers p = abundantNumbers |> Array.exists(fun x -> Array.contains (p - x) abundantNumbers)
    {1..n}
    |> Seq.where(canBeWrittenAsTheSumOfTwoAbundantNumbers >> not)
    |> Seq.sum
    |> string

let rec getPermutationsWithPDigits (s : string) p =
    seq {
        for i in {0..9} |> Seq.where (string >> s.Contains >> not) do 
        match p with
        | 1 -> yield (s + (string i))
        | _ -> yield! (getPermutationsWithPDigits (s + (string i)) (p - 1))
    }

let problem24 n =
    getPermutationsWithPDigits "" 10
    |> Seq.item n

let problem25 n =
    let fibonacci = Seq.unfold(fun (x, y) -> Some(x + y, (y, x + y))) (0I, 1I)
    let indexedFibonacci = fibonacci |> Seq.zip (Seq.initInfinite((+) 1) |> Seq.skip 1)
    indexedFibonacci
    |> Seq.map (fun (x, y) -> (x, string y))
    |> Seq.find(fun (x, y) -> y.Length >= n)
    |> fst
    |> string

// The only state we need to persist is the remainder; we throw
// away the actual result of the division.
// Note that if the remainder becomes 0, the next iteration will be 0
// and it will report a cycle of size 1. This doesn't matter for Problem 26.
let rec recurringCycleIteration accumulatedList divisor =
    let tenLastRemainder = List.last accumulatedList*10
    let remainder = tenLastRemainder - (int (tenLastRemainder/divisor))*divisor
    let index = accumulatedList |> List.tryFindIndex ((=) remainder)
    if index.IsSome
    then List.length accumulatedList - index.Value
    else recurringCycleIteration (accumulatedList @ [remainder]) divisor

let problem26 n =
    {2..n-1}
    |> Seq.maxBy (recurringCycleIteration [1])
    |> string

// Some type annotations can be removed
// Some sequences can be {1..n} instead of seq {1..n}.
// A lot of lambda functions can be simplified for example
// Seq.initInfinite((+) 1) instead of Seq.initInfinite(fun x -> x + 1)
// and Seq.maxBy snd instead of Seq.maxBy(fun x -> snd x)
// Maxby seems very useful for simplifying structure
// You can reduce the line count by declaring a function further up,
// then avoiding brackets around it when using it
let problem27 cap =
    let isPrime n =
        (n > 0)
        && { 2..(n |> (float >> sqrt >> int)) }
        |> Seq.forall (fun x -> n % x <> 0)
    let consecutivePrimes a b =
        Seq.initInfinite((+) 1)
        |> Seq.find(fun n -> (n*n + a*n + b) |> isPrime |> not)
    seq {
        for a in -(cap-1)..(cap-1) do
        for b in -cap..cap do
        yield (a*b, consecutivePrimes a b)
    }
    |> Seq.maxBy snd |> fst |> string

// The first row is the total of the four sequences, evaluated at each successive ring.
// 4 is used at the beginning because each of the four sequences needs to begin with 1.
// The second row is the difference between each successive number and its
// predecessor; the third row is the same logic applied again.
// Note the numbers 4, 20, 32 on the left here. These are used in the function.
//  4   24  76  160 276
//      20  52  84  116
//          32  32  32
let problem28 n =
    Seq.initInfinite (fun _ -> 32)
    |> Seq.scan (+) 20
    |> Seq.scan (+) 4
    |> Seq.skip 1
    |> Seq.take n
    |> Seq.sum
    |> (+) 1
    |> string

let problem29 (n : int32) =
    seq {
        for a in 2I..(bigint n) do
        for b in 2..n do
        yield a**b
    }
    |> Seq.distinct
    |> Seq.length
    |> string

let digits n = n |> string |> Seq.map (string >> int)

let problem30 n =
    let max = (pown 9 n)*n

    {2..max}
    |> Seq.where (fun x -> x = (x |> digits |> Seq.sumBy(fun y -> (pown y n))))
    |> Seq.sum
    |> string

// let problem32 =
//     let rec getPermutationsOneToNine (s : string) p =
//         seq {
//             for i in {1..9} |> Seq.where (string >> s.Contains >> not) do 
//             match p with
//             | 1 -> yield (s + (string i))
//             | _ -> yield! (getPermutationsOneToNine (s + (string i)) (p - 1))
//         }
//     let skipTakeDigits x y = Seq.skip x >> Seq.take y >> stringConcatFromCharSeq >> int
//     let allPermutations = (getPermutationsOneToNine "" 9)
//     allPermutations
//     |> Seq.collect(fun x ->
//         seq {
//             for productDigits in {3..7} do
//             for multiplierDigits in {1..(8-productDigits)} do
//             let multiplicandDigits = 9 - productDigits - multiplierDigits
//             let multiplicand = x |> skipTakeDigits 0 multiplicandDigits
//             let multiplier = x |> skipTakeDigits multiplicandDigits multiplierDigits
//             let product = x |> skipTakeDigits (multiplicandDigits + multiplierDigits) productDigits
//             if (multiplicand * multiplier = product)
//             then yield product
//         }
//     )
//     |> Seq.distinct
//     |> Seq.sum
//     |> string

// let problem33 =
//     let twoDigitFractions =
//         seq {
//             for x in 10..99 do
//             for y in x..99 do                   // consider using x+1 instead
//             yield (x, y)
//         }
    

//         // let x1 = digits x |> Seq.head
//         // let x2 = digits x |> Seq.skip 1 |> Seq.head
//         // let y1 = digits y |> Seq.head
//         // let y2 = digits y |> Seq.skip 1 |> Seq.head
//         // let a1, a2 =
//         //     if (x1 = y1)
//         //     then
//         //         5, 6
//         //     else
//         //         7, 8
//         // yield 5

let problem35 n =
    let isCircularPrime x =
        if (x |> isPrime |> not) then
            false
        else
            let digitsX = digits x
            let numDigits = (digitsX |> Seq.length) - 1
            {1..numDigits}
            |> Seq.forall (fun y ->
                Seq.concat [
                    (digitsX |> Seq.skip y);
                    (digitsX |> Seq.take y)
                ]
                |> stringConcatFromIntSeq
                |> int
                |> isPrime
            )
    {2..n-1}
    |> Seq.where isCircularPrime
    |> Seq.length
    |> string

let problem36 n =
    let isPalindrome s = s = (s |> Seq.rev |> stringConcatFromCharSeq)
    {1..n-1}
    |> Seq.where (fun x ->
        isPalindrome (System.Convert.ToString(x, 2))
        && isPalindrome (System.Convert.ToString(x, 10))
    )
    |> Seq.sum
    |> string

let truncatableInBothDirections n =
    let nStr = string n
    {1..((nStr |> String.length) - 1)}
    |> Seq.forall(fun x ->
        nStr |> Seq.take x |> stringConcatFromCharSeq |> int |> isPrime
        && nStr |> Seq.skip x |> stringConcatFromCharSeq |> int |> isPrime
    )

let problem37 =
    {11..1000000}
    |> Seq.where isPrime
    |> Seq.where truncatableInBothDirections
    |> Seq.take 11      // This 11 and the 11 at the top of this function have nothing to do with each other.
    |> Seq.toList
    |> Seq.sum
    |> string

let problem38 =
    let panDigitalOneToNine s = {'1'..'9'} |> Seq.forall (fun x -> s |> Seq.contains x)
    let numberStrings =
        {1..99999}
        |> Seq.collect (fun x ->
            {1..9}
            |> Seq.map (fun y ->
                {1..y}
                |> Seq.map (( * ) x >> string)
                |> Seq.reduce (+)
            )
        )
        |> Seq.where (fun x -> x.Length = 9 && panDigitalOneToNine x)
    numberStrings
    |> Seq.max
    |> string

// TODO: There has to be a better way to do anything than
// by yielding a sequence of units to get the length of it.
let problem39 n =
    {3..n-1}
    |> Seq.maxBy (fun p ->
        seq {
            for c in 1..(p-2) do
            for b in 1..(p-c-1) do
            let a = p-c-b
            if a*a+b*b=c*c then yield ()
        }
        |> Seq.length
    )
    |> string

let problem40 n =
    let getDigitAtIndex i =
        Seq.initInfinite(fun z -> z + 1)
        |> Seq.collect digits
        |> Seq.item i
    {0..n}
    |> Seq.map (pown 10 >> (+) -1 >> getDigitAtIndex)
    |> Seq.reduce (*)
    |> string

let rec getPermutationsWithPDigitsBackwards max (s : string) p =
    seq {
        for i in {max .. -1 .. 1} |> Seq.where (string >> s.Contains >> not) do 
        match p with
        | 1 -> yield (s + (string i))
        | _ -> yield! (getPermutationsWithPDigitsBackwards max (s + (string i)) (p - 1))
    }

let problem41 =
    let isPandigital n =
        let nStr = string n
        {1..(String.length nStr)}
        |> Seq.map (string >> Seq.head)
        |> Seq.forall(fun x -> nStr |> Seq.contains x)
    {9 .. -1 .. 1}
    |> Seq.collect (fun x -> getPermutationsWithPDigitsBackwards x "" x)
    |> Seq.map int
    |> Seq.find (fun x -> isPrime x && isPandigital x)
    |> string

// let problem43 =
//     getPermutationsWithPDigits "" 10
//     |> Seq.where(fun x ->
//         {0..6}
//         |> Seq.forall(fun y ->
//             x
//             |> Seq.skip (y + 1)
//             |> Seq.take 3
//             |> stringConcatFromCharSeq
//             |> int
//             |> (fun z -> z % (primes |> Seq.item y) = 0)
//         )
//     )
//     |> Seq.sumBy int64
//     |> string

let pentagonal n = (n*(3L*n-1L))/2L
let customUnfold f state =
    Seq.unfold (fun x -> Some(x, f x)) state
let pentagonals = customUnfold ((+) 1L) 1L |> Seq.map pentagonal

// let problem44 =
    // seq {
    //     for x in customUnfold ((+) 1L) 1L |> Seq.map pentagonal |> Seq.takeWhile (fun a -> a < 1000000L) do
    //     for y in customUnfold ((+) 1L) 1L |> Seq.map pentagonal |> Seq.takeWhile (fun b -> b < x) do
    //     if (
    //         (pentagonals |> Seq.contains (x+y))
    //         && (pentagonals |> Seq.contains (abs (x-y)))
    //     )
    //     then
    //         yield abs (x-y)
    // }
//     |> Seq.reduce min
//     |> string

seq {
    for x in ((customUnfold ((+) 1L) 1L) |> Seq.map pentagonal |> (Seq.takeWhile (fun a -> a < 1000000L))) do
    for y in ((customUnfold ((+) 1L) 1L) |> Seq.map pentagonal |> (Seq.takeWhile (fun b -> b < x))) do
    if (
        (pentagonals |> Seq.contains (x+y))
        && (pentagonals |> Seq.contains (abs (x-y)))
    )
    then
        yield abs (x-y)
}
|> Seq.iter (printfn "%i")
//printfn "%s" problem44

//Seq.initInfinite ((+) 1) |> Seq.map pentagonal |> Seq.take 20 |> Seq.iter (printfn "%i")

let problem48 n =
    {1..n}
    |> Seq.sumBy (fun x -> (bigint x) ** x)
    |> string
    |> (fun x -> x |> Seq.skip (x.Length - 10))
    |> stringConcatFromCharSeq

let validate (problemNumber : int) actualOutput expectedOutput =
    "Problem "
    + (string problemNumber)
    + ". "
    +   match expectedOutput with
        | null -> "ATTEMPT (" + actualOutput + ")";
        | expected ->
            match actualOutput with
            | output when output = expected -> "PASS (" + output + ")";
            | output -> "FAIL (" + output + "/" + expected + ")";
    |> printfn "%s"

do validate 1 (problem1 10) "23"
do validate 1 (problem1 1000) "233168"
do validate 2 (problem2 100) "44"
do validate 2 (problem2 4000000) "4613732"
do validate 3 (problem3 13195L) "29"
do validate 3 (problem3 600851475143L) "6857"
do validate 4 (problem4 2) "9009"
do validate 4 (problem4 3) "906609"
// TODO: Calculate 5 using prime factors.
// do validate 5 problem5 "10" "2520"
// do validate 5 problem5 "20" "232792560"
do validate 6 (problem6 10) "2640"
do validate 6 (problem6 100) "25164150"
do validate 7 (problem7 6) "13"
do validate 7 (problem7 10001) "104743"
// Skip 8 due to large input.
// Skip 9 because it takes a long time.
do validate 9 (problem9 1000) "31875000"
do validate 10 (problem10 10L) "17"
// Skip this because it's too slow.
// do validate 10 problem10 "2000000" "142913828922"
// Skip 11 due to large input.
do validate 12 (problem12 5) "28"
do validate 12 (problem12 500) "76576500"
// Skip 13 due to large input.
// TODO: Validation for 14 is to make sure the sequence from 13 to 1 contains 10 terms.
// Skip 14 because it takes a long time.
// do validate 14 problem14 "1000000" "837799"
do validate 15 (problem15 2) "6"
do validate 15 (problem15 20) "137846528820"
do validate 16 (problem16 15) "26"
do validate 16 (problem16 1000) "1366"

// Skip 17-19 due to large input.
do validate 20 (problem20 10) "27"
do validate 20 (problem20 100) "648"
// No first part for 21.
do validate 21 (problem21 10000) "31626"
// Skip 22 due to large input.
// TODO: Optimise 23.
// do validate 23 problem23 "28123" "0"
// TODO: Optimise
// do validate 24 problem24 "1000000" "2783915460"
do validate 25 (problem25 1000) "4782"
do validate 26 (problem26 10) "7"
do validate 26 (problem26 1000) "983"
do validate 27 (problem27 1000) "-59231"
do validate 28 (problem28 2) "101"
do validate 28 (problem28 500) "669171001"
do validate 29 (problem29 5) "15"
do validate 29 (problem29 100) "9183"
do validate 30 (problem30 4) "19316"
do validate 30 (problem30 5) "443839"
// TODO: Optimise 32.
// do validate 32 problem32 "45228"
do validate 35 (problem35 100) "13"
do validate 35 (problem35 1000000) "55"
do validate 36 (problem36 1000000) "872187"
do validate 37 problem37 "748317"
do validate 38 problem38 "932718654"
do validate 39 (problem39 1000) "840"
do validate 40 (problem40 6) "210"
do validate 41 problem41 "7652413"
// Skip 43 because it's evaluated ahead of time
//do validate 43 problem43 "16695334890"

do validate 48 (problem48 10) "0405071317"
do validate 48 (problem48 1000) "9110846700"