open System

let problem1 =
    seq { 1..999 }
    |> Seq.where(fun x -> x % 3 = 0 || x % 5 = 0)
    |> Seq.sum

let problem2 =
    let fibonacci = Seq.unfold(fun (x, y) -> Some(x + y, (y, x + y)))

    Seq.concat [
        seq {1; 2};
        fibonacci (1, 2)
    ]
    |> Seq.takeWhile(fun x -> x <= 4000000)
    |> Seq.where(fun x -> x % 2 = 0)
    |> Seq.sum

let problem3 =
    let primeFactors n =
        Seq.unfold(fun x ->
            if x = 1L then None else
                let divisor = seq { 2L..x } |> Seq.find(fun y -> x % y = 0L)
                Some(divisor, x / divisor)
        ) n

    let l = primeFactors 600851475143L |> Seq.toList

    primeFactors 600851475143L
    |> Seq.max

[<EntryPoint>]
let main argv =
    let problem : int = Int32.Parse(argv.[0])
    let result : string =
        match problem with
        | 1 -> problem1.ToString()
        | 2 -> problem2.ToString()
        | 3 -> problem3.ToString()
        | _ -> "Hello World from F#!"
    Console.WriteLine(result)
    0
