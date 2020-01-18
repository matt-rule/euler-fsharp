open System

let problem1 =
    seq { 1..999 }
    |> Seq.where(fun x -> x % 3 = 0 || x % 5 = 0)
    |> Seq.sum

let problem2 : int =
    let fibonacci = Seq.unfold(fun x -> if x > 4 then Some(3) else None)
    fibonacci 10

[<EntryPoint>]
let main argv =
    let problem : int = Int32.Parse(argv.[0])
    let result : string =
        match problem with
        | 1 -> problem1.ToString()
        | 2 -> "2"
        | 3 -> "3"
        | _ -> "Hello World from F#!"
    Console.WriteLine(result)
    0
