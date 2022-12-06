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


        let startStateRaw = input
                            |> List.takeWhile (fun s -> s.Substring(0,2) |> (<>) " 1")

        let numberOfStacks = startStateRaw
                             |> List.last
                             |> Seq.filter ((=) '[')
                             |> Seq.length

        let startState = [0 .. numberOfStacks - 1]
                         |> List.map (fun i -> startStateRaw
                                               |> List.map (fun s -> s.Substring(1 + 4 * i,1))
                                               |> List.filter ((<>) " ")
                                     )
                         |> List.toArray

        let parseToCommands (s : string) =
            let split = s.Split(' ')
            List.replicate (int split.[1]) (int split.[3] - 1, int split.[5] - 1)

        let executeCommand (state : string list array) (command : int * int) : string list array =
            let movedLetter = state[fst command].Head
            let newFrom = state[fst command].Tail
            let newTo = movedLetter :: state[snd command]
            Array.fill state (fst command) 1 newFrom
            Array.fill state (snd command) 1 newTo
            state

        let orderList = input
                        |> List.partition (fun s -> s.StartsWith('m'))
                        |> fst

        //let completedState = orderList
        //                     |> List.collect parseToCommands
        //                     |> List.fold executeCommand startState

        //let result1 = completedState
        //              |> Array.map List.head
        //              |> Array.reduce (+)





        let startState9001 = [0 .. numberOfStacks - 1]
                             |> List.map (fun i -> startStateRaw
                                                   |> List.map (fun s -> s.Substring(1 + 4 * i,1))
                                                   |> List.filter ((<>) " ")
                                         )
                             |> List.toArray

        let parseToCommands9001 (s : string) =
            let split = s.Split(' ')
            int split.[1], int split.[3] - 1, int split.[5] - 1

        let executeCommand9001 (state : string list array) (command : int * int * int) : string list array =
            let quantity, fra, til = command
            let movedLetters = state[fra] |> List.take quantity
            let newFrom = state[fra] |> List.rev |> List.take (state[fra].Length - quantity) |> List.rev
            let newTo = movedLetters @ state[til]
            Array.fill state (fra) 1 newFrom
            Array.fill state (til) 1 newTo
            state

        let completedState9001 = orderList
                                 |> List.map parseToCommands9001
                                 |> List.fold executeCommand9001 startState9001

        let result2 = completedState9001
                      |> Array.map List.head
                      |> Array.reduce (+)

        result2
        |> printfn "%A"

        //(result1,result2)
        //||> printfn "%A,%A"

        Assert.Pass()

    [<Test>]
    member _.``6`` () =
        let dag = TestContext.CurrentContext.Test.MethodName
        let input = System.IO.File.ReadAllLines (sprintf @"C:\Users\STP\source\repos\econtra\AoC\AdventOfCode\Data\2022\%s.txt" dag)

        let result1 = input
                      |> Array.head
                      |> Seq.windowed 4
                      |> Seq.findIndex (fun a -> a |> Array.distinct |> Array.length = 4)
                      |> (+) 4 // Læg 4 til, da windowed starter på fjerde indgang

        let result2 = input
                      |> Array.head
                      |> Seq.windowed 14
                      |> Seq.findIndex (fun a -> a |> Array.distinct |> Array.length = 14)
                      |> (+) 14 // Læg 14 til, da windowed starter på fjerde indgang


        (result1,result2)
        ||> printfn "%A,%A"


        Assert.Pass()

    [<Test>]
    member _.``7`` () =
        let dag = TestContext.CurrentContext.Test.MethodName
        let input = System.IO.File.ReadAllLines (sprintf @"C:\Users\STP\source\repos\econtra\AoC\AdventOfCode\Data\2022\%s.txt" dag)

        let result1 = 0
        let result2 = 0

        (result1,result2)
        ||> printfn "%A,%A"


        Assert.Pass()
