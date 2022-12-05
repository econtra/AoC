module AoC2022

open FsUnit
open NUnit.Framework
open System
open System.Diagnostics
open System.IO

type Hand =
    | Rock
    | Paper
    | Scissors
    member this.Points (versus : Hand) =
        match this, versus with
            | Rock, Scissors
            | Scissors, Paper
            | Paper, Rock
                -> 6
            | Rock, Paper
            | Paper, Scissors
            | Scissors, Rock
                -> 0
            | _
                -> 3
    member this.Value =
        match this with
            | Rock -> 1
            | Paper -> 2
            | Scissors -> 3

[<TestFixture>]
type ``Results`` () =

    [<Test>]
    member _.``1`` () =
        let dag = TestContext.CurrentContext.Test.MethodName
        let input = System.IO.File.ReadAllLines (sprintf @"C:\Users\STP\source\repos\econtra\AoC\AdventOfCode\Data\2022\%s.txt" dag)
                    |> Array.toList



        let result1 = input
                      |> List.fold (
                                        fun state s -> if s = "" then
                                                         0 :: state
                                                       else
                                                         (state.Head + int s) :: state.Tail
                                   )
                                   [0]
                      |> List.sortDescending



        result1.Head
        |> printfn "%A"

        result1
        |> List.take 3
        |> List.sum
        |> printfn "%A"

        Assert.Pass()

    [<Test>]
    member _.``2`` () =
        let dag = TestContext.CurrentContext.Test.MethodName
        let input = System.IO.File.ReadAllLines (sprintf @"C:\Users\STP\source\repos\econtra\AoC\AdventOfCode\Data\2022\%s.txt" dag)

        let parseToHands (entry : string array) =
            let parse (s : string) =
                match s with
                    | "A"
                    | "X"
                        -> Rock
                    | "B"
                    | "Y"
                        -> Paper
                    | "C"
                    | "Z"
                        -> Scissors
            parse entry.[0], parse entry.[1]

        let parseToHands2 (entry : string array) =
            let my =match entry.[0], entry.[1] with
                    | "A", "Y"
                    | "B", "X"
                    | "C", "Z"
                        -> Rock
                    | "B", "Y"
                    | "C", "X"
                    | "A", "Z"
                        -> Paper
                    | "C", "Y"
                    | "A", "X"
                    | "B", "Z"
                        -> Scissors
            let theirs = match entry.[0] with
                         | "A"
                             -> Rock
                         | "B"
                             -> Paper
                         | "C"
                             -> Scissors
            theirs,my

        let result = input
                     |> Array.map (fun s -> s.Split(" "))
                     |> Array.map parseToHands
                     |> Array.map (fun hands -> ((snd hands).Points (fst hands)) + (snd hands).Value)

        let resulx = input
                     |> Array.map (fun s -> s.Split(" "))
                     |> Array.map parseToHands2
                     |> Array.map (fun hands -> ((snd hands).Points (fst hands)) + (snd hands).Value)



        result
        |> Array.sum
        |> printfn "%A"

        resulx
        |> Array.sum
        |> printfn "%A"

        Assert.Pass()

    [<Test>]
    member _.``3`` () =
        let dag = TestContext.CurrentContext.Test.MethodName
        let input = System.IO.File.ReadAllLines (sprintf @"C:\Users\STP\source\repos\econtra\AoC\AdventOfCode\Data\2022\%s.txt" dag)

        let charToInt c =
            let conv n = int n - int 'a' + 1
            if c |> Char.IsUpper then
                c |> Char.ToLower |> conv |> (+) 26
            else
                conv c


        let sackPairs = input
                        |> Array.map (fun s -> let halfLenght   = s.Length/2
                                               let first        = s.Substring(0,halfLenght) |> Set.ofSeq
                                               let second       = s.Substring(halfLenght,halfLenght) |> Set.ofSeq

                                               first,second
                                     )

        let result1 = sackPairs
                      |> Array.map (fun t -> t ||> Set.intersect)
                      |> Array.map (fun s -> s.MaximumElement |> charToInt)
                      |> Array.sum
                      |> printfn "%A"


        let result2 = input
                      |> Array.chunkBySize 3
                      |> Array.map (fun group -> group
                                                 |> Array.map Set.ofSeq
                                                 |> Set.intersectMany
                                   )
                      |> Array.map (fun s -> s.MaximumElement |> charToInt)
                      |> Array.sum
                      |> printfn "%A"



        Assert.Pass()

    [<Test>]
    member _.``4`` () =
        let dag = TestContext.CurrentContext.Test.MethodName
        let input = System.IO.File.ReadAllLines (sprintf @"C:\Users\STP\source\repos\econtra\AoC\AdventOfCode\Data\2022\%s.txt" dag)

        let elfPairs = input
                       |> Array.map (fun s -> let split = s.Split(',')
                                                          |> Array.map(fun elf -> let borders = elf.Split('-')
                                                                                                |> Array.map int
                                                                                                |> Array.sort
                                                                                  [borders |> Array.min .. borders |> Array.max]
                                                                                  |> Set.ofSeq
                                                                      )
                                              split.[0], split.[1]
                                    )

        let result1 = elfPairs
                      |> Array.map (fun (p1,p2) -> let union = Set.union p1 p2
                                                   if union = p1 || union = p2 then true else false)
                      |> Array.filter ((=) true)
                      |> Array.length

        let result2 = elfPairs
                      |> Array.map (fun pair -> if pair ||> Set.intersect |> Set.isEmpty then false else true)
                      |> Array.filter ((=) true)
                      |> Array.length

        (result1,result2)
        ||> printfn "%A,%A"


        Assert.Pass()

    [<Test>]
    member _.``5`` () =
        let dag = TestContext.CurrentContext.Test.MethodName
        let input = System.IO.File.ReadAllLines (sprintf @"C:\Users\STP\source\repos\econtra\AoC\AdventOfCode\Data\2022\%s.txt" dag)
                    |> Array.toList

        let result1 = 0
        let result2 = 0

        (result1,result2)
        ||> printfn "%A,%A"
        Assert.Pass()
