open System.IO

let input =
  File.ReadAllLines(@"./inputs/day1.txt")
  |> Array.toList
  |> List.map int

let rec f1 acc =
  function
  | []
  | [ _ ] -> acc
  | x :: xs ->
    let acc =
      match x < xs.Head with
      | true -> acc + 1
      | false -> acc

    f1 acc xs

let rec f2 acc =
  function
  | []
  | [ _ ]
  | [ _; _ ]
  | [ _; _; _ ] -> acc
  | a :: b :: c :: d :: xs ->
    let acc =
      match (a + b + c) < (b + c + d) with
      | true -> acc + 1
      | false -> acc

    f2 acc (b :: c :: d :: xs)


;;

f1 0 input |> printfn "Part 1: %i"
f2 0 input |> printfn "Part 2: %i"
