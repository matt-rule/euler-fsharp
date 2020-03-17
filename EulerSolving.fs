namespace EulerApp

open System

module EulerSolving =
    let flip f x y = f y x

    let problem1 n =
        {1..n-1}
        |> Seq.where(fun x -> x % 3 = 0 || x % 5 = 0)
        |> Seq.sum
        |> string

    let problem2 n =
        Seq.unfold(fun (x, y) -> Some(x + y, (y, x + y))) (0, 1)
        |> Seq.takeWhile ((>=) n)
        |> Seq.where(flip (%) 2 >> ((=) 0))
        |> Seq.sum
        |> string

    // Check whether this handles 12 correctly because 2 is a prime factor of it twice (2 squared = 4).
    let problem3 n =
        n
        |> Seq.unfold(fun x ->
            if x = 1L then None else
                let y = {2L..x} |> Seq.find((%) x >> (=) 0L)
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
            |> Seq.map (( * ) x >> string)
            |> Seq.where (fun y -> y = (y |> seq |> Seq.rev |> stringConcatFromCharSeq))
            |> Seq.map int
        )
        |> Seq.max
        |> string

    let isPrime x =
        x > 1 && (
            {2..(float >> sqrt >> int) x}
            |> Seq.forall ((%) x >> (<>) 0)         // Consider (fun y -> x % y <> LanguagePrimitives.GenericZero)
        )

    let primes = Seq.initInfinite((+) 1) |> Seq.where isPrime

    let primeFactors n =
        (
            Seq.unfold (fun p ->
            if p = 1
            then None
            else
                primes
                |> Seq.find((%) p >> (=) 0)
                |> (fun x -> Some(x, p / x))
            ) n
        )
        |> Seq.distinct

    let rec numberOfTimesDivisible x y n =
        if (x % y <> 0)
        then n
        else numberOfTimesDivisible (x/y) y (n+1)

    // TODO: Replace primeFactors with this.
    // Written at a later date: Is this function faster?
    let primeFactorisation n =
        primes
        |> Seq.takeWhile ((>=) n)
        |> Seq.map (fun x -> (x, numberOfTimesDivisible n x 0))
        |> Seq.where (snd >> ((<>) 0))

    let intSqrt = float >> sqrt >> int

    let customUnfold f state =
        Seq.unfold (fun x -> Some(x, f x)) state

    let problem5 n =
        let factorisations =
            [|1..n|]
            |> Seq.map primeFactorisation
            |> Seq.collect id
        let largestPowerForEachPrime =
            primes
            |> Seq.takeWhile ((>=) n)
            |> Seq.map (fun x ->
                factorisations
                |> Seq.where (fst >> ((=) x))
                |> Seq.maxBy snd
            )
        largestPowerForEachPrime
        |> Seq.map (fun (x, y) -> pown x y)
        |> Seq.reduce ( * )
        |> string

    let problem6 n =
        let naturalNumbers =
            Seq.initInfinite((+) 1)
            |> Seq.take n
        let sumOfSquares = Seq.sumBy ((fun x -> x*x)) naturalNumbers
        let squareOfSum = pown (Seq.sum naturalNumbers) 2
        abs(sumOfSquares - squareOfSum)
        |> string

    let isPrime64 l =
        l > 1L && (
            {2L..(float >> sqrt >> int64) l}
            |> Seq.forall (fun y -> l % y <> 0L)        // Consider (fun y -> x % y <> LanguagePrimitives.GenericZero)
        )

    let problem7 n =
        let naturalNumbers = Seq.initInfinite((+) 1)
        naturalNumbers
        |> Seq.where isPrime
        |> Seq.zip naturalNumbers
        |> Seq.find(fun (x, y) -> x = n)
        |> snd
        |> string

    // TODO: Seq.windowed can be used in other places too
    let problem8 (fileLines : string []) n =
        let productOfChars =
            Array.map (string >> int64)
            >> Array.reduce (*)
        fileLines
        |> String.concat ""
        |> Seq.windowed n
        |> Seq.map productOfChars
        |> Seq.max
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

    // TODO: Consider the triangular number sequence on problem 45.

    let problem11 (fileLines : string []) n =
        let numberArray =
            fileLines
            |> Array.map (fun s -> s.Split ' ' |> Array.map int32)
        seq {
            for a in 0..(n-5) do
                for b in 0..(n-5) do
                    yield seq {
                        for x in 0..3 -> numberArray.[a+x].[b+x]
                    }
                    yield seq {
                        for x in 0..3 -> numberArray.[a+4-x].[b+x]
                    }
                for b in 0..(n-1) do
                    yield seq {
                        for x in 0..3 -> numberArray.[a+x].[b]
                    }
                    yield seq {
                        for x in 0..3 -> numberArray.[b].[a+x]
                    }
        }
        |> Seq.map (Seq.reduce (*))
        |> Seq.max
        |> string

    let problem12 n =
        // Triangular numbers.
        Seq.initInfinite ((+) 1)
        |> Seq.scan (+) 0
        |> Seq.find(fun x ->
            // Get all divisors for x up to sqrt(x)
            // This might count the square root twice
            {1..(intSqrt x)}
            |> Seq.where((%) x >> (=) 0)
            |> Seq.length > n / 2
        )
        |> string

    let bigIntDigits = string >> Seq.map (string >> int)

    let problem13 fileLines =
        fileLines
        |> Seq.map bigint.Parse
        |> Seq.reduce (+)
        |> bigIntDigits
        |> Seq.take 10
        |> stringConcatFromIntSeq
        |> string
        
    let problem14 (n : int64) =
        let collatz n = if n % 2L = 0L then (n / 2L) else (n*3L + 1L)
        let rec collatzSequenceLength count n =
            if n = 1L
            then count + 1
            else collatzSequenceLength (count + 1) (collatz n)
        {1L..n-1L}
        |> Seq.maxBy (collatzSequenceLength 0)
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

    let problem17 n =
        let oneToNineteen = [| 3; 3; 5; 4; 4; 3; 5; 5; 4; 3; 6; 6; 8; 8; 7; 7; 9; 8; 8 |]
        let twentyToNinety = [| 6; 6; 5; 5; 5; 7; 6; 6 |]
        let rec letterCount num =
            if num = 1000 then 11
            else if num > 99 then (letterCount (num/100)) + 7 + (if (letterCount (num%100)) > 0 then 3 + (letterCount (num%100)) else 0) 
            else if num > 19 then twentyToNinety.[num/10-2] + letterCount (num%10)
            else if num > 0 then oneToNineteen.[num-1]
            else 0
        {1..n}
        |> Seq.sumBy letterCount
        |> string

    let maxTotalForTriangle (fileLines : string []) =
        let addMaxXToY ((x1, x2), y) = y + max x1 x2
        let addPairwiseMax x y =
            Array.zip (Array.pairwise x) y
            |> Array.map addMaxXToY
        fileLines
        |> Array.map(fun s -> s.Split(' ') |> Array.map int)
        |> Array.rev
        |> Array.reduce addPairwiseMax
        |> Array.exactlyOne
        |> string

    let problem18 = maxTotalForTriangle

    let problem19() =
        let isLeapYear n = (n % 4 = 0) && (n % 100 <> 0 || n % 400 = 0)
        let daysInMonth y m =
            let daysInFeb = if isLeapYear y then 29 else 28
            [| 31; daysInFeb; 31; 30; 31; 30; 31; 31; 30; 31; 30; 31 |].[m]
        seq {
            for y in 1901..2000 do
                for m in 0..11 ->
                    daysInMonth y m
        }
        |> Seq.scan (fun x y -> (x + y) % 7) 0
        |> Seq.where ((=) 0)
        |> Seq.length
        |> string

    let problem20 (n : int32) =
        {1I..(bigint n)}
        |> Seq.fold (fun x y -> bigint.Multiply (x, y)) 1I
        |> sumOfDigits
        |> string

    let sumOfProperDivisors n =
        {2..(intSqrt n)}
        |> Seq.where(fun y -> n % y = 0)
        |> Seq.sumBy(fun x -> if (x = n / x) then x else x + n / x)
        |> ((+) 1)

    let problem21 n =
        {1..n-1}
        |> Seq.where(fun x -> (sumOfProperDivisors >> sumOfProperDivisors) x = x && sumOfProperDivisors x <> x)
        |> Seq.sum
        |> string

    let wordScore =
        Seq.sumBy (int >> ((+) (1 - int 'A')))

    let problem22 (fileText : string) =
        fileText.Replace("\"", "").Split(',')
        |> Array.sort
        |> Array.map wordScore
        |> Seq.zip (Seq.initInfinite((+) 1))
        |> Seq.sumBy (fun (x, y) -> x * y)
        |> string

    let problem23 n =
        let abundantNumbers = 
            {1..n}
            |> Seq.where (fun x -> sumOfProperDivisors x > x)
            |> Seq.toArray
        let mutable sumsOfAbundantPairs = Array.replicate n false
        for index1 in {0..abundantNumbers.Length-1} do
            for index2 in {index1..abundantNumbers.Length-1} do
                if ((abundantNumbers.[index1] + abundantNumbers.[index2]) <= n) then
                    sumsOfAbundantPairs.[abundantNumbers.[index1] + abundantNumbers.[index2] - 1] <- true
        {1..n}
        |> Seq.where(fun x -> not sumsOfAbundantPairs.[x-1])
        |> Seq.sum
        |> string

    let getNextPermutation n (arr : 'a array) =
        let i =
            {n-2 .. -1 .. 0}
            |> Seq.find (fun x -> arr.[x] < arr.[x+1])
        let j =
            {n-1 .. -1 .. i}
            |> Seq.find (fun x -> arr.[x] > arr.[i])
        
        Array.concat [|
            (arr |> Array.take i);
            [| arr.[j] |];
            arr
            |> Array.skip (i + 1)
            |> Array.map (fun x -> if x = arr.[j] then arr.[i] else x)
            |> Array.rev
        |]


    // let getNextPermutation n (arr : 'a array) =
    //     let i1 =
    //         {n-2 .. -1 .. 0}
    //         |> Seq.find (fun i -> arr.[i] < arr.[i+1])
    //     let v2 =
    //         arr
    //         |> Array.skip (i1 + 1)
    //         |> Array.where ((<) arr.[i1])
    //         |> Array.min
    //     let i2 = arr |> Array.findIndex ((=) v2)
    //     Array.concat [|
    //         (arr |> Array.take i1);
    //         [| arr.[i2] |];
    //         arr
    //         |> Array.skip i1
    //         |> Array.where ((<>) arr.[i2])
    //         |> Array.sort
    //     |]

    let problem24 n =
        customUnfold (getNextPermutation 10) [| 0..9 |]
        |> Seq.item (n-1)
        |> Seq.map string
        |> String.concat ""

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
        let mutable cachedPrimes : (int array) =
            Seq.replicate 50000 -1
            |> Array.ofSeq
        let tryCachedPrime n =
            if n < 0 || n >= 50000
            then
                isPrime n
            else
                if cachedPrimes.[n] = -1
                then
                    do cachedPrimes.[n] <- (if isPrime n then 1 else 0)
                (cachedPrimes.[n] = 1)
        let consecutivePrimes a b =
            Seq.initInfinite((+) 1)
            |> Seq.find(fun n -> (n*n + a*n + b) |> tryCachedPrime |> not)
        seq {
            for a in -(cap-1)..(cap-1) do
            for b in -cap..cap ->
                (a*b, consecutivePrimes a b)
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
            for b in 2..n -> a**b
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

    let problem31 (n : int) =
        let coins = [| 1; 2; 5; 10; 20; 50; 100; 200 |]
        let rec countStep (currentTotal : int) (currentIndex : int) =
            seq {
                for i in { currentIndex..((coins |> Array.length) - 1) } do
                    match (currentTotal + (coins.[i])) with
                    | x when x = n -> yield ()
                    | x when x < n -> yield! (countStep (currentTotal + (coins |> Array.item i)) i)
                    | _ -> do ()
            }
        countStep 0 0
        |> Seq.length
        |> string

    let problem32() =
        let rec getPermutationsOneToNine (s : string) p =
            seq {
                for i in {1..9} |> Seq.where (string >> s.Contains >> not) do 
                match p with
                | 1 -> yield (s + (string i))
                | _ -> yield! (getPermutationsOneToNine (s + (string i)) (p - 1))
            }
        let skipTakeDigits x y = Seq.skip x >> Seq.take y >> stringConcatFromCharSeq >> int
        let allPermutations = (getPermutationsOneToNine "" 9)
        allPermutations
        |> Seq.collect(fun x ->
            seq {
                for productDigits in {3..7} do
                for multiplierDigits in {1..(8-productDigits)} do
                let multiplicandDigits = 9 - productDigits - multiplierDigits
                let multiplicand = x |> skipTakeDigits 0 multiplicandDigits
                let multiplier = x |> skipTakeDigits multiplicandDigits multiplierDigits
                let product = x |> skipTakeDigits (multiplicandDigits + multiplierDigits) productDigits
                if (multiplicand * multiplier = product)
                then yield product
            }
        )
        |> Seq.distinct
        |> Seq.sum
        |> string

    let fractionsAreEqual a b =
        (fst a) * (snd b) = (snd a) * (fst b)

    let multiplyFractions a b =
        ((fst a) * (fst b), (snd a) * (snd b))

    let lowestCommonTerms fraction =
        let divisor =
            {(snd fraction).. -1 .. 1}
            |> Seq.find(fun x -> ((fst fraction) % x = 0) && ((snd fraction) % x = 0))
        ((fst fraction) / divisor, (snd fraction) / divisor)

    let isCuriousFraction t =
        let numFstDigit = (fst t) / 10
        let numSndDigit = (fst t) % 10
        let denFstDigit = (snd t) / 10
        let denSndDigit = (snd t) % 10

        if (numSndDigit = 0 && denSndDigit = 0)
        then false
        else
            (numSndDigit = denSndDigit && fractionsAreEqual ((fst t), (snd t)) (numFstDigit, denFstDigit))
            || (numSndDigit = denFstDigit && fractionsAreEqual ((fst t), (snd t)) (numFstDigit, denSndDigit))
            || (numFstDigit = denSndDigit && fractionsAreEqual ((fst t), (snd t)) (numSndDigit, denFstDigit))
            || (numFstDigit = denFstDigit && fractionsAreEqual ((fst t), (snd t)) (numSndDigit, denSndDigit))

    let twoDigitFractions =
        seq {
            for num in 10..99 do
            for den in (num+1)..99 -> (num, den)
        }

    let curiousFractions = twoDigitFractions |> Seq.where isCuriousFraction

    let problem33() =
        curiousFractions
        |> Seq.reduce multiplyFractions
        |> lowestCommonTerms
        |> snd
        |> string

    let factorial n =
        if n = 0 then 1
        else {1..n} |> Seq.reduce ( * )
            
    let problem34 n =
        let sumOfDigitFactorials =
            digits >> Seq.map factorial >> Seq.sum
        {3..n}
        |> Seq.where(fun x -> x = (sumOfDigitFactorials x))
        |> Seq.sum
        |> string

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

    let problem37() =
        {11..1000000}
        |> Seq.where isPrime
        |> Seq.where truncatableInBothDirections
        |> Seq.take 11      // This 11 and the 11 at the top of this function have nothing to do with each other.
        |> Seq.toList
        |> Seq.sum
        |> string

    let problem38() =
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

    let problem41() =
        let isPandigital n =
            let nStr = string n
            {1..(String.length nStr)}
            |> Seq.map (string >> Seq.head)
            |> Seq.forall(flip Seq.contains nStr)
        {9 .. -1 .. 1}
        |> Seq.collect (fun x -> getPermutationsWithPDigitsBackwards x "" x)
        |> Seq.map int
        |> Seq.find (fun x -> isPrime x && isPandigital x)
        |> string

    let problem42 (fileText : string) n =
        let firstNTriangularNumbers =
            Seq.initInfinite ((+) 1)
            |> Seq.scan (+) 0
            |> Seq.take n
            |> Array.ofSeq
        fileText.Replace("\"", "").Split(',')
        |> Seq.map wordScore
        |> Seq.where(flip Seq.contains firstNTriangularNumbers)
        |> Seq.length
        |> string

    let problem43() =
        let x =
            [| 0L..9L |]
            |> Seq.unfold (fun state ->
                if (state |> (Seq.map string >> String.concat "")) = "9876543210"
                then None
                else
                    let nextState = getNextPermutation 10 state
                    Some (nextState, nextState)
            )
        x
        |> Seq.map (Seq.map string >> String.concat "")
        |> Seq.takeWhile (int64 >> ((>) 9876543210L))
        |> Seq.where(fun x ->
            {0..6}
            |> Seq.forall(fun y ->
                x
                |> Seq.skip (y + 1)
                |> Seq.take 3
                |> stringConcatFromCharSeq
                |> int
                |> (fun z -> z % (primes |> Seq.item y) = 0)
            )
        )
        |> Seq.sumBy int64
        |> string

    let pentagonal n = (n*(3L*n-1L))/2L
    let pentagonals = customUnfold ((+) 1L) 1L |> Seq.map pentagonal

    let rec binarySearchStep (arr : int64[]) (x : int64) (indexLowerBound : int) (indexUpperBound : int) =
        if indexUpperBound - indexLowerBound < 2 then
            x = arr.[indexLowerBound] || x = arr.[indexUpperBound]
        else
            let midPointIndex = abs(indexLowerBound - indexUpperBound) / 2 + indexLowerBound
            if x = arr.[midPointIndex] then
                true
            else
                let newLower, newUpper =
                    if x > arr.[midPointIndex] then
                        midPointIndex, indexUpperBound
                    else
                        indexLowerBound, midPointIndex
                binarySearchStep arr x newLower newUpper

    // Assumes arr is sorted
    let binarySearch (arr : int64[]) (x : int64) =
        binarySearchStep arr x 0 (Array.length arr - 1)

    let problem44 n =
        let pentagonalsBelowN = pentagonals |> Seq.takeWhile ((>) n) |> Array.ofSeq
        let isPentagonalBelowN = binarySearch pentagonalsBelowN
        seq {
            for diff in pentagonalsBelowN do
                for pj in pentagonalsBelowN do
                    if (isPentagonalBelowN (diff + pj)) && (isPentagonalBelowN (pj*2L + diff)) then
                       yield diff
        }
        |> Seq.head
        |> string

    let triangular n = n*(n+1L)/2L
    let hexagonal n = n*(2L*n-1L)

    // Find first after n
    let problem45 n limit=
        let pentagonalsBelowLimit = customUnfold ((+) 1L) 1L |> Seq.map pentagonal |> Seq.takeWhile ((>) limit) |> Array.ofSeq
        let triangularsBelowLimit = customUnfold ((+) 1L) 1L |> Seq.map triangular |> Seq.takeWhile ((>) limit) |> Array.ofSeq
        let hexagonalsBelowLimit = customUnfold ((+) 1L) 1L |> Seq.map hexagonal |> Seq.takeWhile ((>) limit) |> Array.ofSeq
        hexagonalsBelowLimit
        |> Seq.where (binarySearch pentagonalsBelowLimit)
        |> Seq.where (binarySearch triangularsBelowLimit)
        |> Seq.find((<) n)
        |> string

    let isSquare n =
        {1..intSqrt n}
        |> Seq.exists(fun x -> x*x = n)

    let canBeWrittenAsSumOfPrimeAndTwiceASquare n =
        {1..n-1}
        |> Seq.where isPrime
        |> Seq.exists(fun x -> (n-x) % 2 = 0 && ((n-x)/2) |> isSquare)

    let problem46() =
        Seq.initInfinite((+) 1)
        |> Seq.skip 1
        |> Seq.where(fun x -> x % 2 = 1 && not (isPrime x))
        |> Seq.find (canBeWrittenAsSumOfPrimeAndTwiceASquare >> not)
        |> string

    // TODO: Replace the bit inside the seq with a recursive function which
    // takes the "factors" array, count, and i as params
    let problem47 n limit =
        let mutable factors = Array.replicate limit 0
        let rec firstN count i =
            seq {
                if i >= limit
                then
                    ()
                else
                    let nextCount =
                        if factors.[i] = 0 then
                            for j in {i .. i .. (limit - 1)} do
                                factors.[j] <- (factors.[j] + 1)
                            0
                        elif factors.[i] = n then
                            count + 1
                        else
                            0
                    if nextCount = n then
                        yield (i - 3)
                    yield! firstN nextCount (i + 1)
            }
        firstN 0 2 
        |> Seq.head
        |> string
            
            
        // let mutable count = 0
        // seq {
        //     for i in {2..limit-1} do
        //         if factors.[i] = 0 then
        //             count <- 0
        //             let mutable val_ = i
        //             while (val_ < limit) do
        //                 factors.[val_] <- (factors.[val_] + 1)
        //                 val_ <- (val_ + i)
        //         elif factors.[i] = n then
        //             count <- (count + 1)
        //             if count = n then
        //                 yield (i - 3)
        //         else
        //             count <- 0
        // }

    let problem48 n =
        {1..n}
        |> Seq.sumBy (fun x -> (bigint x) ** x)
        |> string
        |> (fun x -> x |> Seq.skip (x.Length - 10))
        |> stringConcatFromCharSeq

    let isPermutation x y =
        (x |> digits |> Seq.sort |> stringConcatFromIntSeq) = (y |> digits |> Seq.sort |> stringConcatFromIntSeq)

    let problem49 s =
        seq {
            for x in 1000..9999 do
                if isPrime x then
                    for y in 1..((9999-x)/2) do
                        if (isPermutation x (x+y)) && (isPermutation x (x+y*2)) && isPrime (x+y) && isPrime (x+y*2) then
                            yield (stringConcatFromIntSeq (Seq.concat [digits x; (digits (x+y)); (digits (x+y*2))]))
        }
        |> Seq.find((<>) s)

    let arrayPrimesBelowN n =
        primes
        |> Seq.takeWhile ((>) n)
        |> Array.ofSeq

    //do primesBelowOneMillion |> Array.iter (printfn "%i")

    let rec iteratePrimes (primesArray : int array) (n : int) (indicesSoFar : int) (total : int) (indexToAdd : int) : (int*int) seq =
        seq {
            if indexToAdd >= (primesArray |> Array.length)
            then ()
            else
                let newTotal = total + primesArray.[indexToAdd]
                if newTotal >= n
                then ()
                else
                    if isPrime newTotal then yield (newTotal, indicesSoFar + 1)
                    yield! iteratePrimes primesArray n (indicesSoFar + 1) newTotal (indexToAdd + 1)
        }

    let problem50 n =
        let primesArray = arrayPrimesBelowN n
        [0..(n-1)]
        |> Seq.collect (iteratePrimes primesArray n 0 0)
        |> Seq.maxBy snd
        |> fst
        |> string

    //let problem50 n =
        // generate a list of primes below one million
        // for each prime in the list p
        //     create a sequence q of numbers from p to list.length
        //         fold over q with the following function:
        //             fun x ->
                //         add x to the total
                //             if total is over 1 million then None
                //             if total is prime then
                //                 yield a tuple: (t = total, u = number of primes which were summed)
        // select the tuple in the above sequence with max u
        // take t
        // convert to string

