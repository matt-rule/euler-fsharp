open System

let problem1 =
    seq { 1..999 }
    |> Seq.where(fun x -> x % 3 = 0 || x % 5 = 0)
    |> Seq.sum
    |> string

let problem2 =
    Seq.unfold(fun (x, y) -> Some(x + y, (y, x + y))) (0, 1)
    |> Seq.takeWhile(fun x -> x <= 4000000)
    |> Seq.where(fun x -> x % 2 = 0)
    |> Seq.sum
    |> string

let problem3 =
    600851475143L
    |> Seq.unfold(fun x ->
        if x = 1L then None else
            let y = seq { 2L..x } |> Seq.find(fun y -> x % y = 0L)
            Some(y, x / y)
    )
    |> Seq.max
    |> string

let problem4 =
    seq { 1..999 }
    |> Seq.collect(fun x ->
        seq { x+1..999 }
        |> Seq.map (fun y -> x*y |> string)
        |> Seq.where (fun y -> y = (y |> seq |> Seq.rev |> String.Concat))
        |> Seq.map(Int32.Parse)
    )
    |> Seq.max
    |> string

let problem5 =
    Seq.initInfinite(fun x -> x + 1)
    |> Seq.where (fun x ->
        seq {1..20}
        |> Seq.forall(fun y -> x % y = 0)
    )
    |> Seq.head
    |> string

let answers = [|
    (problem1, "233168")
    (problem2, "4613732")
    (problem3, "6857")
    (problem4, "906609")
    (problem5, "232792560")
|]

answers
|> Array.forall(fun (x, y) -> x = y)
|> Console.WriteLine
answers
|> Array.where(fun (x, y) -> x <> y)
|> String.Concat
|> Console.WriteLine