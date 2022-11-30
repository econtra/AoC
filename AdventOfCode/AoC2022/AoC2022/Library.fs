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
        let input = System.IO.File.ReadAllLines (sprintf @"H:\AdventOfCode\Data\2022\%i.txt" dag)
                    |> Array.map (fun s -> s.Substring(0,1),s.Substring(1) |> int)
                    |> Array.fold (fun s t -> match fst t with | "+" -> s + (snd t) | _ -> s - (snd t)) 0

        let result = input

        printfn "%A" result
        Assert.Pass()

    [<TestCase(2)>]
    member _.``2`` (dag : int) =
        let input = System.IO.File.ReadAllLines (sprintf @"H:\AdventOfCode\Data\2022\%i.txt" dag)
                    |> Seq.ofArray

        let result = 2

        printfn "%A" result
        Assert.Pass()


