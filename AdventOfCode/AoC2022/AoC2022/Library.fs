namespace AoC2022

open FsUnit
open NUnit.Framework
open System
open System.Diagnostics
open System.IO

[<TestFixture>]
type ``Results`` () =

    let testfun x = x + 2


    [<TestCase(1)>]
    member _.``1`` (dag : int) =
        let input = System.IO.File.ReadAllLines (sprintf @"C:\Users\STP\source\repos\econtra\AoC\AdventOfCode\Data\2022\%i.txt" dag)
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

    [<TestCase(2)>]
    member _.``2`` (dag : int) =
        let input = System.IO.File.ReadAllLines (sprintf @"C:\Users\STP\source\repos\econtra\AoC\AdventOfCode\Data\2022\%i.txt" dag)

        let result = input

        printfn "%A" result
        Assert.Pass()
