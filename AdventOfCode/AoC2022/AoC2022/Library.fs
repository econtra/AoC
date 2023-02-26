module AoC2022

open FsUnit
open NUnit.Framework
open System
open System.Diagnostics
open System.IO
open System.Collections.Generic

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




type Command =
    | IntoDir of string
    | FileSize of int
    | Out

type Dir =
    {
        Name   : string
        Size   : int
    }

type Tree =
    {
        X      : int
        Y      : int
        Height : int
    }
    member this.moveN =
        {this with Y = this.Y + 1}
    member this.moveS =
        {this with Y = this.Y - 1}
    member this.moveW =
        {this with X = this.X - 1}
    member this.moveE =
        {this with X = this.X + 1}

type Point =
    {
        Name : string option
        X : int
        Y : int
        Value : int option
    }
    member this.moveN =
        {this with Y = this.Y + 1}
    member this.moveS =
        {this with Y = this.Y - 1}
    member this.moveW =
        {this with X = this.X - 1}
    member this.moveE =
        {this with X = this.X + 1}
    member this.moveNE =
        {this with X = this.X + 1; Y = this.Y + 1}
    member this.moveSE =
        {this with X = this.X + 1; Y = this.Y - 1}
    member this.moveNW =
        {this with X = this.X - 1; Y = this.Y + 1}
    member this.moveSW =
        {this with X = this.X - 1; Y = this.Y - 1}
    /// This is also the 1-norm
    static member distanceTaxi (p : Point) (q : Point) =
        (p.X - q.X |> abs) + (p.Y - q.Y |> abs)
    /// This is also the infty-norm
    static member distanceChessboard (p : Point) (q : Point) =
        max (p.X - q.X |> abs) (p.Y - q.Y |> abs)
    static member Origin =
        {Name = None; X = 0; Y = 0; Value = None}

type Rope =
    {
        Head : Point
        Tail : Point
        TailVisits : Point list
    }

type LongRope =
    {
        Parts : Point list
        TailVisits : Point list
    }

type CPUCommand =
    | NOOP
    | ADDX of int

type CPUbeh =
    | AddCycle
    | ChangeCounter of int

type CPU =
    {
        Cycle : int
        X     : int
    }
    static member init =
        {Cycle = 0; X = 1}

type Monkey =
    {
        Number : int
        Items : int list
        Operation : int -> int
        Test : int -> int
        InspectionCount : int
    }

type Node =
    {
        Name : string option
        Destinations : Node list
        isVisited : bool
    }

type IntOrList =
    | Integer of int
    | List of IntOrList list

type BFSstate =
    {
        Queue : (Point * int) list
        VisitedPoints : Point list
    }
    static member Start (startNode : Point) =
        {
            Queue = [startNode,0]
            VisitedPoints = []
        }

let manhattanDistance (p : int * int) (q : int * int) =
    let x,y = p
    let a,b = q
    (abs (x - a), abs (y - b)) ||> (+)

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

        let n1 = 4
        let result1 = input
                      |> Array.head
                      |> Seq.windowed n1
                      |> Seq.findIndex (Array.distinct >> Array.length >> ((=) n1))
                      |> (+) 4 // Læg 4 til, da windowed starter på fjerde indgang

        let n2 = 14
        let result2 = input
                      |> Array.head
                      |> Seq.windowed n2
                      |> Seq.findIndex (Array.distinct >> Array.length >> ((=) n2))
                      |> (+) n2 // Læg 14 til, da windowed starter på fjortende indgang


        (result1,result2)
        ||> printfn "%A,%A"


        Assert.Pass()

    [<Test>]
    member _.``7`` () =
        let dag = TestContext.CurrentContext.Test.MethodName
        let input = System.IO.File.ReadAllLines (sprintf @"C:\Users\STP\source\repos\econtra\AoC\AdventOfCode\Data\2022\%s.txt" dag)
                    |> Array.toList

        let parse (line : string) : Command option =
            match line with
                | l when l = "$ cd .." -> Out |> Some
                | l when l.Substring(0,4) = "$ cd" -> line.Substring(5) |> IntoDir |> Some
                | _ -> if Char.IsDigit(line.Substring(0,1) |> char) then line.Split(' ') |> Array.head |> int |> FileSize |> Some else None

        let commands = input |> List.choose parse

        let folder (state : Dir list * Dir list) (command : Command) : Dir list * Dir list =
            let openDirs,closedDirs = state
            match command with
                | IntoDir name -> ({Name = name; Size = 0} :: openDirs), closedDirs
                | Out -> openDirs.Tail,(openDirs.Head :: closedDirs)
                | FileSize i -> (openDirs |> List.map (fun d -> {d with Size = d.Size + i})), closedDirs

        let maxDirSize = 100000

        let dirs = commands
                   |> List.fold folder (List.empty,List.empty)
                   ||> List.append

        let result1 = dirs
                      |> List.filter (fun d -> d.Size <= maxDirSize)
                      |> List.sumBy (fun d -> d.Size)


        let diskSpace = 70000000
        let unusedSpaceNeeded = 30000000
        let usedSpace = dirs |> List.find (fun d -> d.Name = "/") |> (fun d -> d.Size)

        let result2 = dirs
                      |> List.filter (fun d -> diskSpace - usedSpace + d.Size > unusedSpaceNeeded)
                      |> List.minBy (fun d -> d.Size)
                      |> (fun d -> d.Size)

        (result1,result2)
        ||> printfn "%A,%A"


        Assert.Pass()

    [<Test>]
    member _.``8`` () =
        let dag = TestContext.CurrentContext.Test.MethodName
        let input = System.IO.File.ReadAllLines (sprintf @"C:\Users\STP\source\repos\econtra\AoC\AdventOfCode\Data\2022\%s.txt" dag)
                    |> Array.toList

        let trees = input
                    |> List.rev
                    |> List.mapi  (fun i s -> s |> Seq.mapi (fun j t -> {X = j; Y = i; Height = t |> System.Globalization.CharUnicodeInfo.GetDigitValue}))
                    |> Seq.collect id
                    |> Seq.toList

        let isVisible (trees : Tree list) (tree : Tree): bool =
            let cross = trees
                        |> List.filter (fun t -> t.X = tree.X || t.Y = tree.Y)
            (
                (cross |> List.exists (fun t -> t.X = tree.X && t.Y > tree.Y && t.Height >= tree.Height)) // tree blocking above
                &&
                (cross |> List.exists (fun t -> t.X = tree.X && t.Y < tree.Y && t.Height >= tree.Height)) // tree blocking below
                &&
                (cross |> List.exists (fun t -> t.X < tree.X && t.Y = tree.Y && t.Height >= tree.Height)) // tree blocking left
                &&
                (cross |> List.exists (fun t -> t.X > tree.X && t.Y = tree.Y && t.Height >= tree.Height)) // tree blocking right
            )
            |> not

        let visibleTrees = trees
                           |> List.filter (isVisible trees)

        let scenicScore (trees : Tree list) (tree : Tree): int =
            let cross = trees
                        |> List.filter (fun t -> t.X = tree.X || t.Y = tree.Y)

            let up = cross |> List.filter   (fun t -> t.Y > tree.Y) |> List.takeWhile (fun t -> t.Height < tree.Height) |> List.length |>             (fun i -> if i = (cross |> List.filter (fun t -> t.Y > tree.Y) |> List.length) then i else i+1)
            let left = cross |> List.filter (fun t -> t.X < tree.X) |> List.rev |> List.takeWhile (fun t -> t.Height < tree.Height) |> List.length |> (fun i -> if i = (cross |> List.filter (fun t -> t.X < tree.X) |> List.length) then i else i+1)
            let down = cross |> List.filter (fun t -> t.Y < tree.Y) |> List.rev |> List.takeWhile (fun t -> t.Height < tree.Height) |> List.length |> (fun i -> if i = (cross |> List.filter (fun t -> t.Y < tree.Y) |> List.length) then i else i+1)
            let right = cross |> List.filter(fun t -> t.X > tree.X) |> List.takeWhile (fun t -> t.Height < tree.Height) |> List.length |>             (fun i -> if i = (cross |> List.filter (fun t -> t.X > tree.X) |> List.length) then i else i+1)

            [up;down;left;right]
            |> List.reduce (*)

        let result1 = visibleTrees.Length
        let result2 = trees
                      |> List.maxBy (scenicScore trees)
                      |> scenicScore trees

        (result1,result2)
        ||> printfn "%A,%A"


        Assert.Pass()

    [<Test>]
    member _.``9`` () =
        let dag = TestContext.CurrentContext.Test.MethodName
        let input = System.IO.File.ReadAllLines (sprintf @"C:\Users\STP\source\repos\econtra\AoC\AdventOfCode\Data\2022\%s.txt" dag)
                   |> Array.toList

        let parse (s : string) =
            let split = s.Split(' ')
            List.replicate (int split[1]) split[0]

        let moveRope (state : Rope) (command : string) : Rope =
            let newHead = match command with
                             | "U" -> state.Head.moveN
                             | "D" -> state.Head.moveS
                             | "L" -> state.Head.moveW
                             | "R" -> state.Head.moveE
                             | _ -> failwith "Unknown command"

            let newTail = if Point.distanceChessboard newHead state.Tail > 1 then state.Head else state.Tail

            {state with Head = newHead; Tail = newTail; TailVisits = newTail :: state.TailVisits}

        let pointsVisited = input
                            |> List.collect parse
                            |> List.fold moveRope {Head = Point.Origin; Tail = Point.Origin; TailVisits = [Point.Origin]}
                            |> (fun r -> r.TailVisits |> List.distinct |> List.length)




        let ropeLength = 10

        let movePart (h : Point) (t : Point) : Point =
            match h,t with
                | x,y when (Point.distanceChessboard x y) <= 1 -> t
                | x,y when (x.X = y.X || x.Y = y.Y) -> [y.moveE;y.moveN;y.moveS;y.moveW] |> List.find (fun p -> Point.distanceChessboard x p <= 1)
                | x,y -> [y.moveNE;y.moveNW;y.moveSE;y.moveSW] |> List.find (fun p -> Point.distanceChessboard x p <= 1)

        let rec moveLongRope (rope : Point list) : Point list =
            match rope with
                | [t] -> []
                | h :: t -> let newTailHead = movePart h t.Head
                            newTailHead :: moveLongRope (newTailHead :: t.Tail)

        let moveLongRope (state : LongRope) (command : string) : LongRope =
            let newHead = match command with
                             | "U" -> state.Parts.Head.moveN
                             | "D" -> state.Parts.Head.moveS
                             | "L" -> state.Parts.Head.moveW
                             | "R" -> state.Parts.Head.moveE
                             | _ -> failwith "Unknown command"

            let newTail = moveLongRope (newHead :: state.Parts.Tail)

            {state with Parts = newHead :: newTail; TailVisits = List.last newTail :: state.TailVisits}



        let pointsVisited2 = input
                             |> List.collect parse
                             |> List.fold moveLongRope {Parts = List.replicate ropeLength Point.Origin; TailVisits = [Point.Origin]}
                             |> (fun r -> r.TailVisits |> List.distinct |> List.length)

        let result1 = pointsVisited
        let result2 = pointsVisited2

        (result1,result2)
        ||> printfn "%A,%A"


        Assert.Pass()

    [<Test>]
    member _.``10`` () =
        let dag = TestContext.CurrentContext.Test.MethodName
        let input = System.IO.File.ReadAllLines (sprintf @"C:\Users\STP\source\repos\econtra\AoC\AdventOfCode\Data\2022\%s.txt" dag)
                    |> Array.toList

        let parse (s : string) =
            let split = s.Split(' ')
            match split[0] with
                | "addx" -> [AddCycle; AddCycle; split[1] |> int |> ChangeCounter]
                | "noop" -> [AddCycle]

        let scanner (cpu : CPU) (command : CPUbeh) : CPU =
            match command with
                | AddCycle -> {cpu with Cycle = cpu.Cycle + 1}
                | ChangeCounter x -> {cpu with X = cpu.X + x}

        let states = input
                     |> List.collect parse
                     |> List.scan scanner CPU.init

        let interesting = List.init 6 (fun j -> j * 40 + 20)
                          |> List.map (fun i -> states |> List.find (fun s -> s.Cycle = i) |> (fun cpu -> cpu.X * i))
                          |> List.sum


        let result1 = interesting


        let collapsedStates = states.Tail |> List.distinctBy (fun s -> s.Cycle)

        let folder (crt : string) (cpu : CPU) : string =
            let projectedCycle = cpu.Cycle % 40
            let s = if abs (projectedCycle - 1 - cpu.X) <= 1 then "#" else "."
            crt + s




        let implode (xs:char list) =
            let sb = System.Text.StringBuilder(xs.Length)
            xs |> List.iter (sb.Append >> ignore)
            sb.ToString()

        let crt = collapsedStates
                  |> List.fold folder ""
                  |> Seq.chunkBySize 40
                  |> Seq.map (Array.toList >> implode)
                  |> Seq.toList



        let resultPath = sprintf @"C:\Users\STP\source\repos\econtra\AoC\AdventOfCode\Data\2022\%sresult.txt" dag

        let result2 = sprintf "See %A" resultPath

        (result1,result2)
        ||> printfn "%A,%A"

        System.IO.File.WriteAllLines (resultPath, crt)


        Assert.Pass()

    [<Test>]
    member _.``11`` () =
        let dag = TestContext.CurrentContext.Test.MethodName
        let input = System.IO.File.ReadAllLines (sprintf @"C:\Users\STP\source\repos\econtra\AoC\AdventOfCode\Data\2022\%s.txt" dag)
                    |> Array.toList
                    |> List.chunkBySize 7

        let monkeyParser (monkeyAsStringList : string list) : Monkey =
            let number = (monkeyAsStringList[0].Split(' ')[1]).Substring(0,1) |> int
            let items = (monkeyAsStringList[1].Split(':')[1]).Split(',') |> Array.map int |> Array.toList
            let operation =
                let split = (monkeyAsStringList[2].Split('=')[1]).Split(' ')
                if split[3] = "old" then (fun (x : int) -> x * x) else match split[2], int split[3] with
                                                                       | "+", x -> (+) x
                                                                       | "-", x -> (-) x
                                                                       | "*", x -> (*) x
                                                                       | "/", x -> (/) x
            let test =
                let div = int (monkeyAsStringList[3].Split("by ")[1])
                let ifTrue = int (monkeyAsStringList[4].Split("monkey ")[1])
                let ifFalse = int (monkeyAsStringList[5].Split("monkey ")[1])
                (fun x -> if x % div = 0 then ifTrue else ifFalse)
            {
                Number = number
                Items = items
                Operation = operation
                Test = test
                InspectionCount = 0
            }

        let monkeys = input
                      |> List.map monkeyParser

        let numberOfTurns = 20

        let monkeyTurn (monkeys : Monkey list) (monkeyNumber : int) : Monkey list =
            let monkey = monkeys |> List.find (fun m -> m.Number = monkeyNumber)
            let items = monkey.Items
                        |> List.map monkey.Operation
                        |> List.map (fun i -> (/) (float i) (float 3) |> floor |> int)
                        |> List.groupBy monkey.Test
                        |> dict

            monkeys
            |> List.map (
                            fun m -> if m.Number = monkeyNumber then
                                        {m with Items = []; InspectionCount = m.InspectionCount + monkey.Items.Length}
                                     else
                                        {m with Items = m.Items @ (if items.ContainsKey m.Number then items.Item m.Number else [])}
                        )

        let monkeyBusiness (monkeys : Monkey list) i : Monkey list =
            [0 .. monkeys.Length - 1]
            |> List.fold monkeyTurn monkeys

        let result1 = [1 .. numberOfTurns]
                      |> List.fold monkeyBusiness monkeys
                      |> List.sortByDescending (fun m -> m.InspectionCount)
                      |> List.take 2
                      |> List.map (fun m -> m.InspectionCount)
                      |> List.reduce (*)

        let result2 = 0

        (result1,result2)
        ||> printfn "%A,%A"


        Assert.Pass()

    [<Test>]
    member _.``12`` () =
        let dag = TestContext.CurrentContext.Test.MethodName
        let input = System.IO.File.ReadAllLines (sprintf @"C:\Users\STP\source\repos\econtra\AoC\AdventOfCode\Data\2022\%s.txt" dag)
                    |> Array.toList

        let let2nat (letter : char) = match letter with
                                      | 'S' -> 0
                                      | 'E' -> 25
                                      | _ -> int letter - int 'a'

        let points = input
                     |> List.mapi  (fun i s -> s |> Seq.mapi (fun j t -> {Name = t |> string |> Some; X = i; Y = j; Value = t |> let2nat |> Some}))
                     |> Seq.collect id
                     |> Seq.toList

        let startPoint = points |> List.find (fun p -> p.Name = Some "S")
        let endPoint = points |> List.find (fun p -> p.Name = Some "E")

        let paths = points |> List.map (fun p ->
                                                                let relevantNeigbours = points |> List.filter (fun q -> Point.distanceTaxi q p = 1 && q.Value.Value <= p.Value.Value + 1)
                                                                p,relevantNeigbours
                                       )
                           |> dict

        let generator (state : BFSstate) =
            if state.Queue.IsEmpty then
                None
            else
                let currentPoint,currentLenght = state.Queue.Head
                let newVisitedPoints = currentPoint :: state.VisitedPoints
                let oldQueue = state.Queue
                let relevantNeighbours = paths.Item currentPoint |> List.filter (fun p ->
                                                                                                List.exists ((=) p) newVisitedPoints |> not
                                                                                                &&
                                                                                                List.exists (fun q -> p = fst q) oldQueue |> not
                                                                                     )
                let relevantNeighboursWithTraces = relevantNeighbours |> List.map (fun p -> p,currentLenght + 1)
                let newQueue = oldQueue.Tail @ relevantNeighboursWithTraces
                let newState = {Queue = newQueue; VisitedPoints = newVisitedPoints}
                Some ((if currentPoint.Name = Some "E" then Some (currentPoint,currentLenght) else None),newState)

        let traces = List.unfold generator (BFSstate.Start startPoint)

        let result1 = traces
                      |> List.find (fun trace -> trace.IsSome)
                      |> (fun t -> (snd t.Value))





        let startPoints = points |> List.filter (fun p -> p.Name = (Some "S") || p.Name = (Some "a"))

        let result2 = startPoints
                      |> List.map (fun p -> (List.unfold generator (BFSstate.Start p)))
                      |> List.map (fun p -> p |> List.filter (fun x -> x.IsSome))
                      |> List.map (fun p -> match p with | [] -> Int32.MaxValue | l -> l.Head.Value |> snd)
                      |> List.min

        (result1,result2)
        ||> printfn "%A,%A"


        Assert.Pass()

    [<Test>]
    member _.``13`` () =
        let dag = TestContext.CurrentContext.Test.MethodName
        let input = System.IO.File.ReadAllLines (sprintf @"C:\Users\STP\source\repos\econtra\AoC\AdventOfCode\Data\2022\%s.txt" dag)
                    |> Array.toList
                    |> List.chunkBySize 3
                    |> List.map (fun l -> l |> List.filter (fun s -> s <> ""))

        let removeFirstAndLast (s : string) : string =
            s |> Seq.tail |> Seq.rev |> Seq.tail |> Seq.rev |> Seq.map string |> Seq.reduce (+)

        let removeFirst (s : string) : string =
            s |> Seq.tail |> Seq.map string |> Seq.reduce (+)

        let inline charToInt c = int c - int '0'

        //let rec parser (remainder : string) (currentList : IntOrList list) : IntOrList list =
        //    let current = Seq.head remainder
        //    match current with
        //    | '[' -> let content = remainder |> removeFirstAndLast
        //             (parser newRemainder [])
        //    | _ -> (current |> charToInt |> IntOrList.Integer)  (line |> removeFirst |> parser |> IntOrList.List)

        //let a = input
        //        |> List.map (fun l -> l |> List.map parser)

        let result1 = 0
        let result2 = 0

        (result1,result2)
        ||> printfn "%A,%A"


        Assert.Pass()

    [<Test>]
    member _.``14`` () =
        let dag = TestContext.CurrentContext.Test.MethodName
        let input = System.IO.File.ReadAllLines (sprintf @"C:\Users\STP\source\repos\econtra\AoC\AdventOfCode\Data\2022\%s.txt" dag)
                    |> Array.toList

        let lineToPoints (line : string) : (int * int) list =
            line.Split(" -> ")
            |> Array.map (fun s -> s.Split(",")[0] |> int , s.Split(",")[1] |> int)
            |> Array.toList

        let rockInfo = input
                       |> List.map lineToPoints

        let processRock (endPoints : (int * int) list) =
            let windows = List.windowed 2 endPoints
            windows
            |> List.collect (
                                fun l -> if fst l.[0] = fst l.[1] then
                                            [(min (snd l.[0]) (snd l.[1])) .. (max (snd l.[0]) (snd l.[1]))] |> List.map (fun x -> fst l.[0],x)
                                         else
                                            [(min (fst l.[0]) (fst l.[1])) .. (max (fst l.[0]) (fst l.[1]))] |> List.map (fun x -> x, snd l.[0])
                            )
            |> List.distinct

        let completeRocks = rockInfo
                            |> List.collect processRock

        let minOut  = completeRocks |> List.minBy (fun (x,y) -> x) |> fst |> (+) (-2)//486
        let maxOut  = completeRocks |> List.maxBy (fun (x,y) -> x) |> fst |> (+) 2//582
        let maxDown = completeRocks |> List.maxBy (fun (x,y) -> y) |> snd |> (+) 2//175

        //let initCave = List.init (maxOut * maxDown) (fun i -> {Name = None; X = i % maxOut; Y = (float (i+1)) / (float maxOut) |> ceil |> int; Value = None})
        //               |> List.filter (fun p -> p.X > minOut)

        let initCave = Array2D.init maxOut maxDown (fun x y -> if (completeRocks |> List.exists (fun p -> p = (x,y))) then 1 else 0)



        let rec processCave (cave : int array2d) : int array2d =

            let rec moveSand (position : int * int) : (int * int) =
                match position with
                | x,y when y >= maxDown - 1 -> (0,0)
                | x,y when cave.[x,y + 1] = 0 -> moveSand (x,y + 1)
                | x,y when cave.[x - 1,y + 1] = 0 -> moveSand (x - 1,y + 1)
                | x,y when cave.[x + 1,y + 1] = 0 -> moveSand (x + 1,y + 1)
                | _ -> position

            let startSandPosition = (500,0)
            let endSandPosition = moveSand startSandPosition
            if endSandPosition = (0,0) then
                cave
            else
                 (endSandPosition ||> (Array2D.set cave) <| 2)
                 processCave cave





        let newMaxOut = 1100
        let newMaxDown = maxDown

        let initCave2 = Array2D.init newMaxOut (newMaxDown + 2) (fun x y -> if (completeRocks |> List.exists (fun p -> p = (x,y))) || y=newMaxDown then 1 else 0)

        let rec processCave2 (cave : int array2d) : int array2d =

            let rec moveSand (position : int * int) : (int * int) =
                match position with
                | x,y when cave.[x,y + 1] = 0 -> moveSand (x,y + 1)
                | x,y when cave.[x - 1,y + 1] = 0 -> moveSand (x - 1,y + 1)
                | x,y when cave.[x + 1,y + 1] = 0 -> moveSand (x + 1,y + 1)
                | _ -> position

            let startSandPosition = (500,0)
            let endSandPosition = moveSand startSandPosition
            if endSandPosition = startSandPosition then
                (endSandPosition ||> (Array2D.set cave) <| 2)
                cave
            else
                (endSandPosition ||> (Array2D.set cave) <| 2)
                processCave2 cave

        let result1 = initCave
                      |> processCave
                      |> Seq.cast<int>
                      |> Seq.filter (fun i -> i = 2)
                      |> Seq.length
        let result2 = initCave2
                      |> processCave2
                      |> Seq.cast<int>
                      |> Seq.filter (fun i -> i = 2)
                      |> Seq.length

        (result1,result2)
        ||> printfn "%A,%A"


        Assert.Pass()

    [<Test>]
    member _.``15`` () =
        let dag = TestContext.CurrentContext.Test.MethodName
        let input = System.IO.File.ReadAllLines (sprintf @"C:\Users\STP\source\repos\econtra\AoC\AdventOfCode\Data\2022\%s.txt" dag)
                    |> Array.toList

        let stringToSensorAndBeacon (s : string) : ((int * int) * (int * int)) =
            let split = s.Split('=')
            let a = split[1].Split(',')[0] |> int
            let b = split[2].Split(':')[0] |> int
            let x = split[3].Split(',')[0] |> int
            let y = split[4]               |> int
            ((a,b),(x,y))

        let sensorsAndBeaconsAndDistances = input
                                            |> List.map stringToSensorAndBeacon
                                            |> List.map (fun (sensor,beacon) -> sensor, beacon,manhattanDistance sensor beacon)

        let minOut = sensorsAndBeaconsAndDistances
                     |> List.map (fun ((a,b),_,d) -> a - d)
                     |> List.min

        let maxOut = sensorsAndBeaconsAndDistances
                     |> List.map (fun ((a,b),_,d) -> a + d)
                     |> List.max

        let relevantrow = 2000000

        let noBeacons = [minOut .. maxOut]
                        |> List.map (fun x -> x,relevantrow)
                        |> List.filter (
                                            fun p -> sensorsAndBeaconsAndDistances
                                                     |> List.exists (
                                                                        fun (s,b,d) -> manhattanDistance p s <= d
                                                                                       &&
                                                                                       p <> b
                                                                    )
                                       )

        let result1 = noBeacons.Length

        let minBeaconPos,maxBeaconPos = 0,4000000

        let a = sensorsAndBeaconsAndDistances
                |> List.collect (
                                fun ((a,b),_,d) -> let distancePlusOne = d + 1
                                                   let bl = ([0 .. d] |> List.map (fun i -> a - distancePlusOne + i         , b + i    ))
                                                   let br = ([0 .. d] |> List.map (fun i -> a + i     , b + distancePlusOne - i))
                                                   let tr = ([0 .. d] |> List.map (fun i -> a + distancePlusOne - i         , b - i    ))
                                                   let tl = ([0 .. d] |> List.map (fun i -> a - i     , b - distancePlusOne + i))
                                                   bl @ br @ tr @ tl
                                                   |> List.filter (fun (a,b) -> max a b <= maxBeaconPos && min a b >= minBeaconPos)
                                                   //|> Set.ofList
                            )
                |> List.countBy id
                |> List.sortByDescending (fun ((_,_),c) -> c)
                |> List.head
                |> fst
                |> (fun (a,b) -> 4000000.0 * (float a) + (float b))
                //|> Set.intersectMany

        let result2 = a

        (result1,result2)
        ||> printfn "%A,%A"


        Assert.Pass()


    [<Test>]
    member _.``16`` () =
        let dag = TestContext.CurrentContext.Test.MethodName
        let test = "Steffen Thyge Pedersen"
        let input = System.IO.File.ReadAllText (sprintf @"C:\Users\STP\source\repos\econtra\AoC\AdventOfCode\Data\2022\%s.txt" dag)
                    |> (fun s -> s.Split ',')
                    |> Array.toList

        let firstThreeLetters (name : string) =
            name.Substring (0,3) |> (fun s -> s.ToUpper())

        let firstTwoAndLastLetter (name : string) =
            name.Substring (0,2) + string name[name.Length - 1] |> (fun s -> s.ToUpper())

        let firstAndLastTwoLetters (name : string) =
            name.Substring (0,1) + string name[name.Length - 2] + string name[name.Length - 1] |> (fun s -> s.ToUpper())

        let firstLetterAndTwoX (name : string) =
            name.Substring (0,1) + "XX" |> (fun s -> s.ToUpper())

        let firstTwoLettersAndX (name : string) =
            name.Substring (0,1) + string name[name.Length - 2] + "X" |> (fun s -> s.ToUpper())

        let listOfMethods =
            [
                firstThreeLetters
                firstTwoAndLastLetter
                firstAndLastTwoLetters
                firstLetterAndTwoX
                firstTwoLettersAndX
            ]

        let findUnusedAbbreviation (alreadyInUse : string list) (name : string) =
            let f = listOfMethods
                    |> List.tryFind (fun g -> let candidate = g name
                                              alreadyInUse |> List.exists ((=) candidate) |> not
                                              )
            match f with | Some f -> alreadyInUse @ [f name] | None -> failwith ("KEK")

        let result = input
                     |> List.fold findUnusedAbbreviation List.empty

        let a = 5

        Assert.Pass()
