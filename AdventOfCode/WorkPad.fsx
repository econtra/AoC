module Euler1 =
    let Sum (number : int) =
        List.init number id |> List.filter (fun i -> i % 3 = 0 || i % 5 = 0) |> List.sum
    printfn "%i" <| Sum 1000

module Euler2 =
    let Fibs (length : int) =
        List.init length id |> List.scan (fun s x -> (fst s) + (snd s), (fst s) ) (1,1) |> List.map fst

    Fibs 35 |> List.filter (fun i -> i % 2 = 0) |> List.filter ((>) (int 4e6)) |> List.sum |> (printfn "%i")

module Problem1 =
    let myLast (inputList : 'a list) =
        List.last inputList

    myLast [1; 2; 3; 4]
    myLast ['x';'y';'z']

module Problem2 =
    let myButLast (inputList : 'a list) =
        let listRev = inputList
                      |> List.rev

        listRev.Tail.Head

    myButLast [1; 2; 3; 4]
    myButLast ['a'..'z']

module Problem3 =
    let elementAt (inputList : 'a list) k =
        inputList |> List.item (k-1)

    elementAt [1; 2; 3] 2
    elementAt (List.ofSeq "fsharp") 5

module Problem4 =
    let myLength  (inputList : 'a list) =
        inputList |> List.length

    myLength [123; 456; 789]
    myLength <| List.ofSeq "Hello, world!"

module Problem5 =
    let myRev  (inputList : 'a list) =
        inputList |> List.rev

    myRev <| List.ofSeq ("A man, a plan, a canal, panama!")
    myRev <| List.ofSeq [1,2,3,4]

module Problem6 =
    let rec myIsPalindrome  (inputList : 'a list) =
            inputList |> function | []
                                  | [_] -> true
                                  | h::t when (h=(t |> List.rev).Head) -> myIsPalindrome (t |> List.rev).Tail
                                  | _ -> false

    myIsPalindrome [1;2;3]
    myIsPalindrome <| List.ofSeq "madamimadam"
    myIsPalindrome [1;2;4;8;16;8;4;2;1]

module QueenAttack =
    let QueenAttack (pos : (int * int) * (int * int)) =
        pos |> function | (a,b),(x,y) when (a=x || b=y || (abs(a-x)-abs(b-y)=0) ) -> true
                        | _ -> false

    QueenAttack ((2,3),(2,8))

    let ViggoQueenAttack (pos : (int * int) * (int * int)) =
        pos |> function | (a,b),(x,y) when ((sqrt(abs(float(a-x)) * abs(float(b-y)))) % 1.0) = 0.0 -> true
                        | _ -> false

    ViggoQueenAttack ((2,3),(4,5))

module Proverb =
    let Proverb (input : string list) =
        let rec adder (wip : (string list) * string) =
            wip |> function | [x;y],s -> [],s + "For want of a " + x + " the " + y + " was lost. "
                            | head::tail,s -> (tail, (s + "For want of a " + head + " the " + tail.Head + " was lost. ")) |> adder


        let partResult = adder (input,"") |> snd
        let part2 = partResult + "And all for the want of a " + input.Head + "."
        printfn "%s" part2

    Proverb ["kek";"lul";"lel";"sadge";"weirdga"]

    let ProverbMultiLine (input : string list) =
        let rec adder wip : (string list) =
            wip |> function | [x] -> do printfn "And all for the want of a %s." x
                                     [x]
                            | head::tail -> do printfn "For want of a %s the %s was lost." head tail.Head
                                            tail |> adder
        adder input

    ProverbMultiLine ["kek";"lul";"lel";"sadge";"weirdga"]

    let ProverbListFold (input : string list) =
        input.Tail
        |> List.fold (fun s e -> (e, ["For want of a " + fst s + " the " + e + " was lost."] |> List.append (snd s))) (input.Head,[])
        |> snd
        |> (@) <| ["And all for the want of a " + input.Head + "."]
        |> List.iter (printfn "%s")


    ProverbListFold ["kek";"lul";"lel";"sadge";"weirdga"]

module Anagram =
    let Anagram (word : string) (suggestions : string list) =

        let isAnagram (sug : string) =
            sug
            |> Seq.sort
            |> Seq.compareWith Operators.compare <| Seq.sort word
            |> (=) 0


        suggestions
        |> List.filter isAnagram

    Anagram "listen" ["enlists";"google";"inlets";"banana";"liste";"lisen"]

module Allergies =
    let Allergies (score : int) (item : int) =
        let allergies = Seq.initInfinite (fun i -> 2f**(float32 i)) |> Seq.map int
        let highestAllergyPower = allergies |> Seq.findIndex ((<) score)
        let potentialAllergies = allergies |> Seq.take highestAllergyPower |> List.ofSeq
        let listOfAllergies = potentialAllergies |> List.scanBack (fun x (b,s) -> match s,x with s,x when s>=x -> true,s-x | _ -> false,s) <| (false,score)

        let liste = (potentialAllergies @ [0]) |> List.zip <| listOfAllergies
                                               |> List.filter (snd >> fst)
                                               |> List.map fst

        (List.exists ((=) item) liste),liste


    let test = Allergies 29 2

module PerfectNumbers =
    let Perfect (number : int) =
        Seq.init number id |> Seq.tail |> Seq.filter (((%) number) >> ((=) 0)) |> Seq.sum |> (=) number

    let a = List.init 100 id |> List.tail |> List.map Perfect

module DaysOfChristmas =
    open System
    let OrdinalEnding (number : int) =
        number.ToString() |> function | s when s.EndsWith("11") -> "th"
                                      | s when s.EndsWith("12") -> "th"
                                      | s when s.EndsWith("13") -> "th"
                                      | s when s.EndsWith("1") -> "st"
                                      | s when s.EndsWith("2") -> "nd"
                                      | s when s.EndsWith("3") -> "rd"
                                      | _ -> "th"

    let DaysOfChristmas (words : string list) =
        words |> List.mapi (fun i w -> (i+1).ToString() + OrdinalEnding (i+1) , w)
              |> List.fold (fun s x -> (snd x) :: (fst s),(snd s) + "On the " + (fst x) + " day of Christmas my true love gave to me: " + ((fst s) |> List.rev |> List.foldBack (fun str state -> String.Format("{0}, {1}", state, str)) <| (snd x)) + ". ") (list.Empty,"")
              |> snd

    let test = DaysOfChristmas ["kek";"lul";"lel";"lol";"gorgc"]
    printf "%s" test
    let testList = test.Split('.') |> Array.map (fun s -> s.TrimStart() + ".") |> Array.rev |> Array.tail |> Array.rev
    testList |> Array.iter (printfn "%s")

module Acronym =
    open System
    let Acronym (word : string) =
        word |> List.ofSeq |> List.filter Char.IsUpper |> List.map string |> List.reduce (+)

    Acronym "Black King Bar" |> printfn "%s"

module SaddlePoints =
    let SaddlePoints (A : int [,]) =
        A |> Array2D.iteri (fun i j e -> if Array.forall ((>=) e) A.[i,*] && Array.forall ((<=) e) A.[*,j] then printfn "Saddle point %i in position %i,%i" e i j)

    let arrayOfArrays = [| [| 1; 2 |]; [|0; 1 |] |]
    let twoDimensionalArray = Array2D.init 2 2 (fun i j -> arrayOfArrays.[i].[j])



    let r = System.Random()
    let rows, cols = r.Next(1,10),r.Next(1,10)

    let randomTwoDimensionalArray = Array2D.init rows cols (fun _ _ -> r.Next(25))

    SaddlePoints randomTwoDimensionalArray

module Isogram =
    let IsogramSimple (phrase : string) =
        phrase |> Seq.distinct |> Seq.length |> (=) phrase.Length

    let IsogramAdv (phrase : string) =
        phrase |> List.ofSeq |> List.partition (fun c -> c = char " " || c = char "-") |> (fun (a,b) -> a.Length + (List.distinct b).Length) |>  (=) phrase.Length


    printfn "%b" <| IsogramSimple "lumberjack"
    printfn "%b" <| IsogramAdv "six-year-old"

module CollatzConjecture =
    let Collatz N =
        let rec sekvens n =
            seq {
                match n with
                | 1 -> ()
                | _ ->
                    yield n
                    yield! sekvens (if ((n % 2) = 0) then n/2 else 3*n+1)
            }

        N |> sekvens |> Seq.length

    [12;54;322;35;32;12;6] |> List.map Collatz |> printfn "%A"

module Atbash =
    let Atbash (s : string) =
        let reverse (c : char) =
            (int) c % 32
              |> (*) -1
              |> (+) 27
              |> char

        Seq.map reverse s

    printfn "%A" <| Atbash "cipher"

module BookStore =
    let TotalPrice =
        0m

module PrimeFactors =
    let findSmallestDivisor n =
        Seq.initInfinite (fun i -> i+2) |> Seq.find (fun i -> n % i = 0)

    let factors number =
        let m = int number
        let rec s y = seq {
                            match y with
                                | 1 -> ()
                                | k ->
                                    let d = findSmallestDivisor k
                                    yield d
                                    yield! k/d |> s}
        s m

    factors 901255L |> printfn "%A"

module Lasagna =
    let expectedMinutesInOven = 40
    let remainingMinutesInOven elapsed = expectedMinutesInOven - elapsed
    let preparationTimeInMinutes layers = layers * 2
    let elapsedTimeInMinutes layers elapsed = layers * 2 + elapsed

module SpaceInvaders =

    let listeA = [0 .. 10]

    listeA
    |> List.iter (printfn "%A, that was a great trick")

module Advent2020 =
    module Advent1 =
        let input = System.IO.File.ReadAllLines @"H:\AdventOfCode\fullinput2020_1.txt" |> Array.map int

        let result1 = input
                      |> Array.map (fun i -> input |> Array.tryFind (fun j -> if i+j=2020 then true else false) |> Option.map ((*) i))
                      |> Array.tryFind Option.isSome

        let result2 = input
                      |> Array.map (fun i -> input |> Array.tryFind (fun j -> if i+j=2020 then true else false) |> Option.map ((*) i))
                      |> Array.tryFind Option.isSome

        let a = 5

    module Advent2 =
        let input = System.IO.File.ReadAllLines @"H:\AdventOfCode\fullinput2020_2.txt"

        let isValid (str : string) =
            let split = str.Split(':')
            let letter = split.[0].Split(' ').[1] |> char
            let bounds = split.[0].Split(' ').[0].Split('-') |> Array.map int
            let pw = split.[1] |> Seq.sumBy (fun c -> if c = letter then 1 else 0)
            if pw >= bounds.[0] && pw <= bounds.[1] then 1 else 0


        let result1 = input |> Array.map isValid
                            |> Array.sum

        let isValid2 (str : string) =
            let split = str.Split(':')
            let letter = split.[0].Split(' ').[1] |> char
            let positions = split.[0].Split(' ').[0].Split('-') |> Array.map int
            let pw = split.[1].Trim() |> seq |> Seq.toList
            if (pw.[positions.[0]-1] = letter) <> (pw.[positions.[1]-1] = letter) then 1 else 0

        let result2 = input |> Array.map isValid2
                            |> Array.sum

    module Advent3 =
        let input = System.IO.File.ReadAllLines @"H:\AdventOfCode\fullinput2020_3.txt" |> Seq.toList

        let len = input |> List.length

        let parseLine (str : string) =
            let trees = str |> Seq.mapi (fun i c -> if c = (char "#") then Some i else None)
                            |> Seq.choose id

            let doneLine = List.replicate 250 str |> Seq.collect id
            doneLine |> Seq.toList

        let board = input |> Seq.map parseLine |> Seq.toList

        let boardToSum (board : char list list) (slope : int) =
            board |> List.mapi (fun i l -> if l.[i*slope] = '#' then 1 else 0) |> List.sum

        let result = [1;3;5;7] |> List.map (fun i -> (boardToSum board i))

        let func (l : char list) i =
            let floatindex = (float i) * 0.5
            let index = int floatindex
            l.[index] = '#'

        let result2 = board |> List.mapi (fun i l -> if func l i && i % 2 = 0 then 1 else 0) |> List.sum

        let a = 5

    module Advent4 =

        let input = System.IO.File.ReadAllLines @"H:\AdventOfCode\fullinput2020_4.txt" |> Array.toList

        let folder (state : string list * string) (x : string) =
            if x = "" then (fst state) @ [snd state],"" else (fst state), snd state + " " + x

        let passports = input |> List.fold folder (List.empty, "") ||> (fun l x -> l @ [x])

        type Passport = {
                        BirthYear       : string option
                        IssueYear       : string option
                        ExpirationDate  : string option
                        Height          : string option
                        HairColor       : string option
                        EyeColor        : string option
                        PassportID      : string option
                        CountryID       : string option
                        }

        let parse (passport : string) =
            let split = passport.Split(' ') |> Array.toList
            let func field (str : string) =
                if str.Split(':').[0] = field then str.Split(':').[1] |> Some else None
            {
                BirthYear     = split |> List.tryPick (func "byr")
                IssueYear     = split |> List.tryPick (func "iyr")
                ExpirationDate= split |> List.tryPick (func "eyr")
                Height        = split |> List.tryPick (func "hgt")
                HairColor     = split |> List.tryPick (func "hcl")
                EyeColor      = split |> List.tryPick (func "ecl")
                PassportID    = split |> List.tryPick (func "pid")
                CountryID     = split |> List.tryPick (func "cid")
                }

        let pp = passports |> List.map parse

        let isValid (p : Passport) =
            p.BirthYear     .IsSome &&
            p.IssueYear     .IsSome &&
            p.ExpirationDate.IsSome &&
            p.Height        .IsSome &&
            p.HairColor     .IsSome &&
            p.EyeColor      .IsSome &&
            p.PassportID    .IsSome

        //let isValid2 (p : Passport) =
        //    p.BirthYear.Value.Substring(4) |> int |> (fun y -> 1920 <= y && y <= 2002) &&
        //    p.IssueYear.Value.Substring(4) |> int |> (fun y -> 2010 <= y && y <= 2020) &&
        //    p.ExpirationDate.Value.Substring(4) |> int |> (fun y -> 2020 <= y && y <= 2030) &&
        //    match p.Height.Value.Split(':').[1] with | "cm" -> if p.Height.Value.Split(':').[0] |> int |> (fun y -> 150 <= y && y <= 193) then true else false | "in" -> if p.Height.Value.Split(':').[0] |> int |> (fun y -> 59 <= y && y <= 76) then true else false &&



        let result = pp |> List.filter isValid |> List.length

        let a = 5

    module Advent5 =

        let input = System.IO.File.ReadAllLines @"H:\AdventOfCode\fullinput2020_5.txt"

        let lineParser (str : string) =
            let  a = str.Substring(0,7)
                        .Replace('F','0')
                        .Replace('B','1')
                        |> (fun s -> System.Convert.ToInt64(s,2))

            let  b = str.Substring(7)
                        .Replace('L','0')
                        .Replace('R','1')
                        |> (fun s -> System.Convert.ToInt64(s,2))

            a,b

        let result = input |> Array.map lineParser |> Array.map (fun t -> (fst t) * 8L + (snd t)) |> Array.max

        let chooser (window : int64 []) =
            if window.[0] = window.[1] - 2L then Some (window.[0] + 1L) else None

        let result2 = input |> Array.map lineParser |> Array.map (fun t -> (fst t) * 8L + (snd t)) |> Array.sort |> Array.windowed 2 |> Array.pick chooser

        let a=5

    module Advent6 =

        let input = System.IO.File.ReadAllLines @"H:\AdventOfCode\fullinput2020_6.txt" |> Array.toList

        let folder (state : string list * string) (x : string) =
            if x = "" then (fst state) @ [snd state],"" else (fst state), snd state + x

        let groups = input |> List.fold folder (List.empty, "") ||> (fun l x -> l @ [x])

        let mapper (str : string) =
            str |> Seq.distinct |> Seq.length

        let result1 = groups |> List.map mapper |> List.sum


        let folder2 (state : string list * string) (x : string) =
            if x = "" then (fst state) @ [snd state],"" else (fst state), snd state + " " + x

        let groups2 = input |> List.fold folder2 (List.empty, "") ||> (fun l x -> l @ [x])


        let mapper2 (str : string) =
            let number = str |> Seq.filter ((=) ' ') |> Seq.length
            let answers = str |> Seq.countBy id
            let count = answers |> Seq.filter (fun (k,i) -> i=number) |> Seq.length
            count - 1

        let result2 = groups2 |> List.map mapper2 |> List.sum


        let a = 5

    module Advent7 =
        open System
        let input = System.IO.File.ReadAllLines @"H:\AdventOfCode\fullinput2020_7.txt" |> Array.toList

        let processLine (liste : string list) (bagRule : string) =
            let split = bagRule.Split([|" bags contain "|], StringSplitOptions.None)
            let bag = split.[0]
            let content = split.[1].Split
            [""]

        let result = input |> List.fold processLine [""]

module Advent2021 =

    module Advent1 =

        let input = [|
                        148
                        167
                        168
                        169
                        182
                        188
                        193
                        209
                        195
                        206
                        214
                        219
                        225
                        219
                        211
                        215
                        216
                        195
                        200
                        197
                        226
                        231
                        234
                        248
                        249
                        265
                        262
                        275
                        271
                        283
                        297
                        302
                        306
                        307
                        309
                        313
                        306
                        307
                        310
                        307
                        306
                        284
                        291
                        296
                        297
                        298
                        302
                        297
                        295
                        296
                        297
                        293
                        294
                        295
                        303
                        302
                        304
                        305
                        303
                        288
                        289
                        290
                        291
                        290
                        288
                        302
                        304
                        282
                        283
                        290
                        301
                        304
                        301
                        330
                        337
                        341
                        340
                        337
                        341
                        342
                        343
                        301
                        311
                        312
                        302
                        317
                        318
                        319
                        345
                        347
                        354
                        358
                        356
                        357
                        362
                        361
                        357
                        375
                        376
                        383
                        384
                        382
                        383
                        384
                        391
                        400
                        410
                        413
                        419
                        421
                        424
                        426
                        425
                        427
                        429
                        432
                        433
                        434
                        440
                        446
                        444
                        453
                        461
                        463
                        461
                        464
                        463
                        489
                        486
                        485
                        486
                        499
                        498
                        493
                        494
                        485
                        490
                        499
                        498
                        506
                        510
                        519
                        524
                        520
                        521
                        543
                        550
                        551
                        553
                        564
                        567
                        568
                        572
                        597
                        579
                        592
                        597
                        618
                        620
                        628
                        651
                        674
                        671
                        684
                        719
                        718
                        730
                        727
                        729
                        724
                        720
                        737
                        740
                        753
                        763
                        764
                        762
                        772
                        774
                        773
                        771
                        777
                        774
                        783
                        791
                        792
                        791
                        801
                        822
                        821
                        814
                        811
                        812
                        817
                        835
                        844
                        837
                        839
                        841
                        855
                        872
                        881
                        895
                        910
                        900
                        902
                        913
                        914
                        911
                        917
                        918
                        923
                        933
                        932
                        931
                        915
                        912
                        923
                        939
                        940
                        941
                        943
                        947
                        948
                        931
                        933
                        944
                        933
                        947
                        948
                        949
                        956
                        949
                        948
                        969
                        973
                        989
                        993
                        994
                        993
                        1007
                        1010
                        1020
                        1024
                        1026
                        1031
                        1032
                        1028
                        1045
                        1015
                        1018
                        1024
                        998
                        1000
                        1006
                        994
                        995
                        993
                        1010
                        1011
                        1010
                        1015
                        1021
                        1039
                        1045
                        1041
                        1049
                        1044
                        1050
                        1063
                        1066
                        1051
                        1056
                        1059
                        1060
                        1059
                        1064
                        1067
                        1066
                        1067
                        1069
                        1072
                        1074
                        1077
                        1078
                        1080
                        1086
                        1079
                        1089
                        1093
                        1099
                        1102
                        1103
                        1108
                        1119
                        1138
                        1139
                        1144
                        1148
                        1151
                        1152
                        1151
                        1155
                        1154
                        1153
                        1157
                        1159
                        1164
                        1168
                        1172
                        1171
                        1182
                        1189
                        1190
                        1192
                        1194
                        1196
                        1197
                        1222
                        1216
                        1220
                        1206
                        1205
                        1196
                        1200
                        1210
                        1203
                        1200
                        1212
                        1218
                        1219
                        1205
                        1211
                        1212
                        1213
                        1176
                        1187
                        1174
                        1175
                        1180
                        1181
                        1187
                        1185
                        1211
                        1212
                        1199
                        1231
                        1234
                        1249
                        1250
                        1247
                        1235
                        1234
                        1236
                        1237
                        1266
                        1265
                        1286
                        1289
                        1282
                        1283
                        1273
                        1282
                        1267
                        1271
                        1278
                        1291
                        1307
                        1294
                        1292
                        1320
                        1323
                        1314
                        1317
                        1342
                        1344
                        1319
                        1326
                        1330
                        1329
                        1331
                        1335
                        1338
                        1360
                        1364
                        1381
                        1389
                        1386
                        1387
                        1388
                        1389
                        1391
                        1395
                        1398
                        1404
                        1405
                        1408
                        1412
                        1427
                        1410
                        1425
                        1409
                        1410
                        1412
                        1411
                        1394
                        1406
                        1407
                        1404
                        1413
                        1418
                        1417
                        1442
                        1443
                        1444
                        1445
                        1458
                        1452
                        1453
                        1466
                        1465
                        1466
                        1465
                        1476
                        1484
                        1487
                        1490
                        1494
                        1495
                        1502
                        1498
                        1501
                        1496
                        1498
                        1492
                        1501
                        1499
                        1477
                        1480
                        1483
                        1484
                        1483
                        1488
                        1494
                        1491
                        1489
                        1503
                        1535
                        1544
                        1547
                        1549
                        1550
                        1557
                        1542
                        1540
                        1542
                        1543
                        1544
                        1542
                        1541
                        1546
                        1542
                        1540
                        1526
                        1534
                        1536
                        1549
                        1542
                        1552
                        1553
                        1554
                        1569
                        1568
                        1569
                        1563
                        1560
                        1572
                        1568
                        1544
                        1552
                        1559
                        1563
                        1556
                        1577
                        1594
                        1596
                        1581
                        1580
                        1570
                        1552
                        1560
                        1561
                        1558
                        1559
                        1560
                        1561
                        1560
                        1554
                        1562
                        1565
                        1561
                        1570
                        1571
                        1572
                        1568
                        1565
                        1574
                        1616
                        1607
                        1604
                        1606
                        1609
                        1607
                        1608
                        1609
                        1618
                        1621
                        1622
                        1623
                        1628
                        1637
                        1648
                        1656
                        1653
                        1649
                        1651
                        1654
                        1693
                        1697
                        1705
                        1706
                        1705
                        1712
                        1709
                        1708
                        1715
                        1717
                        1721
                        1723
                        1730
                        1729
                        1740
                        1732
                        1733
                        1740
                        1746
                        1747
                        1744
                        1748
                        1746
                        1745
                        1747
                        1749
                        1750
                        1744
                        1747
                        1749
                        1751
                        1749
                        1751
                        1782
                        1783
                        1767
                        1769
                        1799
                        1777
                        1783
                        1784
                        1786
                        1790
                        1791
                        1802
                        1804
                        1807
                        1824
                        1825
                        1840
                        1852
                        1872
                        1874
                        1871
                        1866
                        1861
                        1860
                        1861
                        1842
                        1840
                        1841
                        1842
                        1841
                        1832
                        1848
                        1851
                        1834
                        1846
                        1844
                        1826
                        1829
                        1840
                        1841
                        1844
                        1848
                        1820
                        1831
                        1833
                        1822
                        1814
                        1819
                        1833
                        1832
                        1824
                        1814
                        1815
                        1818
                        1829
                        1832
                        1833
                        1847
                        1846
                        1848
                        1850
                        1880
                        1889
                        1887
                        1888
                        1881
                        1880
                        1883
                        1885
                        1882
                        1881
                        1868
                        1870
                        1855
                        1862
                        1863
                        1870
                        1866
                        1875
                        1894
                        1896
                        1880
                        1898
                        1917
                        1918
                        1914
                        1919
                        1907
                        1905
                        1893
                        1901
                        1899
                        1909
                        1910
                        1918
                        1921
                        1930
                        1929
                        1935
                        1917
                        1919
                        1917
                        1907
                        1911
                        1910
                        1911
                        1930
                        1958
                        1964
                        1971
                        1972
                        1950
                        1963
                        1967
                        1968
                        1965
                        1973
                        1975
                        1965
                        1963
                        1949
                        1950
                        1953
                        1952
                        1957
                        1974
                        1992
                        2022
                        2021
                        2022
                        2025
                        2024
                        2032
                        2048
                        2074
                        2068
                        2072
                        2068
                        2085
                        2091
                        2111
                        2095
                        2091
                        2089
                        2103
                        2107
                        2117
                        2123
                        2128
                        2136
                        2134
                        2140
                        2141
                        2140
                        2141
                        2122
                        2125
                        2129
                        2142
                        2106
                        2101
                        2104
                        2122
                        2131
                        2156
                        2178
                        2187
                        2182
                        2180
                        2177
                        2178
                        2179
                        2197
                        2198
                        2197
                        2198
                        2202
                        2204
                        2210
                        2211
                        2212
                        2207
                        2206
                        2211
                        2216
                        2209
                        2210
                        2202
                        2207
                        2206
                        2207
                        2210
                        2213
                        2185
                        2187
                        2188
                        2203
                        2206
                        2213
                        2233
                        2238
                        2249
                        2250
                        2252
                        2253
                        2240
                        2246
                        2248
                        2235
                        2237
                        2255
                        2257
                        2235
                        2245
                        2240
                        2246
                        2237
                        2255
                        2282
                        2283
                        2291
                        2304
                        2305
                        2306
                        2309
                        2319
                        2321
                        2323
                        2318
                        2317
                        2316
                        2317
                        2323
                        2314
                        2326
                        2313
                        2314
                        2316
                        2317
                        2320
                        2317
                        2326
                        2322
                        2348
                        2349
                        2335
                        2331
                        2338
                        2343
                        2344
                        2345
                        2335
                        2332
                        2339
                        2340
                        2335
                        2346
                        2347
                        2343
                        2342
                        2344
                        2333
                        2326
                        2351
                        2355
                        2363
                        2366
                        2364
                        2393
                        2410
                        2411
                        2433
                        2440
                        2413
                        2407
                        2403
                        2404
                        2407
                        2427
                        2425
                        2430
                        2446
                        2449
                        2450
                        2448
                        2453
                        2454
                        2456
                        2455
                        2456
                        2455
                        2460
                        2451
                        2464
                        2463
                        2468
                        2471
                        2472
                        2456
                        2467
                        2471
                        2481
                        2505
                        2524
                        2534
                        2533
                        2531
                        2526
                        2527
                        2522
                        2534
                        2535
                        2538
                        2537
                        2538
                        2541
                        2542
                        2544
                        2543
                        2529
                        2519
                        2508
                        2522
                        2526
                        2554
                        2560
                        2554
                        2558
                        2562
                        2576
                        2580
                        2581
                        2589
                        2573
                        2596
                        2600
                        2601
                        2606
                        2607
                        2611
                        2606
                        2624
                        2631
                        2634
                        2663
                        2661
                        2663
                        2676
                        2680
                        2678
                        2680
                        2677
                        2672
                        2685
                        2716
                        2713
                        2690
                        2698
                        2699
                        2716
                        2720
                        2719
                        2720
                        2724
                        2722
                        2724
                        2725
                        2727
                        2733
                        2736
                        2739
                        2740
                        2753
                        2762
                        2763
                        2793
                        2808
                        2787
                        2778
                        2782
                        2771
                        2783
                        2795
                        2803
                        2808
                        2832
                        2833
                        2842
                        2843
                        2839
                        2840
                        2807
                        2808
                        2806
                        2825
                        2836
                        2838
                        2842
                        2848
                        2817
                        2818
                        2821
                        2820
                        2844
                        2859
                        2830
                        2824
                        2825
                        2850
                        2853
                        2852
                        2830
                        2835
                        2839
                        2828
                        2827
                        2828
                        2826
                        2827
                        2815
                        2816
                        2819
                        2818
                        2792
                        2789
                        2793
                        2801
                        2778
                        2779
                        2780
                        2783
                        2808
                        2824
                        2792
                        2811
                        2810
                        2777
                        2774
                        2773
                        2782
                        2781
                        2790
                        2785
                        2789
                        2785
                        2786
                        2785
                        2782
                        2783
                        2785
                        2788
                        2785
                        2790
                        2816
                        2834
                        2835
                        2836
                        2842
                        2846
                        2839
                        2861
                        2863
                        2868
                        2867
                        2864
                        2849
                        2850
                        2851
                        2852
                        2826
                        2815
                        2814
                        2820
                        2821
                        2837
                        2865
                        2866
                        2851
                        2856
                        2861
                        2867
                        2874
                        2871
                        2872
                        2861
                        2842
                        2843
                        2840
                        2872
                        2879
                        2873
                        2881
                        2877
                        2880
                        2873
                        2872
                        2902
                        2906
                        2900
                        2899
                        2911
                        2914
                        2924
                        2925
                        2920
                        2909
                        2901
                        2924
                        2936
                        2938
                        2947
                        2950
                        2962
                        2963
                        2955
                        2953
                        2970
                        2967
                        2947
                        2958
                        2961
                        2962
                        2952
                        2953
                        2951
                        2947
                        2945
                        2952
                        2956
                        2959
                        2961
                        2960
                        2965
                        2966
                        2962
                        2964
                        2972
                        2975
                        2985
                        2982
                        2996
                        2997
                        2991
                        2994
                        2995
                        2994
                        2978
                        2987
                        2994
                        3003
                        2994
                        2993
                        2998
                        3000
                        3004
                        3005
                        3006
                        3024
                        3046
                        3049
                        3053
                        3052
                        3064
                        3067
                        3065
                        3067
                        3068
                        3063
                        3065
                        3067
                        3063
                        3066
                        3068
                        3079
                        3085
                        3086
                        3088
                        3090
                        3093
                        3119
                        3144
                        3143
                        3144
                        3146
                        3159
                        3160
                        3162
                        3164
                        3160
                        3158
                        3143
                        3138
                        3135
                        3140
                        3145
                        3137
                        3134
                        3140
                        3141
                        3135
                        3134
                        3133
                        3123
                        3124
                        3123
                        3124
                        3126
                        3125
                        3109
                        3105
                        3106
                        3120
                        3100
                        3112
                        3114
                        3122
                        3121
                        3120
                        3131
                        3140
                        3141
                        3152
                        3153
                        3154
                        3155
                        3162
                        3160
                        3161
                        3177
                        3190
                        3191
                        3192
                        3194
                        3196
                        3190
                        3187
                        3199
                        3193
                        3200
                        3199
                        3214
                        3212
                        3175
                        3150
                        3153
                        3177
                        3181
                        3165
                        3183
                        3185
                        3187
                        3188
                        3187
                        3201
                        3203
                        3202
                        3200
                        3209
                        3218
                        3217
                        3216
                        3208
                        3204
                        3203
                        3179
                        3185
                        3188
                        3195
                        3196
                        3193
                        3194
                        3211
                        3216
                        3208
                        3210
                        3211
                        3212
                        3204
                        3205
                        3209
                        3232
                        3234
                        3255
                        3254
                        3264
                        3267
                        3269
                        3274
                        3278
                        3290
                        3301
                        3303
                        3305
                        3306
                        3304
                        3305
                        3302
                        3307
                        3308
                        3297
                        3302
                        3307
                        3306
                        3307
                        3306
                        3274
                        3275
                        3276
                        3278
                        3281
                        3280
                        3274
                        3299
                        3298
                        3276
                        3275
                        3278
                        3281
                        3284
                        3285
                        3281
                        3282
                        3244
                        3239
                        3240
                        3217
                        3236
                        3238
                        3240
                        3245
                        3248
                        3255
                        3256
                        3260
                        3261
                        3271
                        3289
                        3302
                        3303
                        3304
                        3305
                        3304
                        3306
                        3307
                        3334
                        3338
                        3341
                        3340
                        3360
                        3354
                        3353
                        3366
                        3369
                        3374
                        3377
                        3368
                        3361
                        3378
                        3385
                        3384
                        3382
                        3386
                        3384
                        3388
                        3381
                        3358
                        3359
                        3363
                        3364
                        3365
                        3366
                        3368
                        3392
                        3419
                        3436
                        3438
                        3452
                        3479
                        3480
                        3483
                        3494
                        3500
                        3511
                        3524
                        3535
                        3560
                        3571
                        3584
                        3600
                        3598
                        3591
                        3593
                        3598
                        3599
                        3598
                        3599
                        3610
                        3608
                        3610
                        3611
                        3615
                        3616
                        3620
                        3606
                        3622
                        3625
                        3629
                        3636
                        3635
                        3640
                        3635
                        3636
                        3634
                        3635
                        3652
                        3654
                        3655
                        3657
                        3642
                        3647
                        3654
                        3661
                        3621
                        3620
                        3621
                        3628
                        3604
                        3597
                        3598
                        3587
                        3588
                        3591
                        3595
                        3594
                        3596
                        3597
                        3598
                        3603
                        3604
                        3605
                        3632
                        3627
                        3630
                        3631
                        3632
                        3642
                        3643
                        3644
                        3649
                        3653
                        3652
                        3653
                        3654
                        3653
                        3669
                        3688
                        3696
                        3697
                        3704
                        3707
                        3710
                        3704
                        3718
                        3721
                        3725
                        3751
                        3757
                        3760
                        3776
                        3775
                        3782
                        3778
                        3792
                        3804
                        3818
                        3819
                        3806
                        3810
                        3795
                        3806
                        3821
                        3823
                        3858
                        3860
                        3858
                        3865
                        3868
                        3875
                        3876
                        3878
                        3888
                        3891
                        3893
                        3884
                        3889
                        3897
                        3891
                        3894
                        3896
                        3924
                        3925
                        3926
                        3927
                        3934
                        3938
                        3966
                        3972
                        3973
                        3977
                        3976
                        4003
                        4017
                        4023
                        4019
                        4017
                        3988
                        3997
                        4010
                        4017
                        4018
                        3994
                        3995
                        3985
                        3987
                        3997
                        4004
                        4003
                        4005
                        4009
                        4007
                        4021
                        4023
                        3998
                        3991
                        4021
                        4020
                        4027
                        4026
                        4030
                        4043
                        4047
                        4048
                        4046
                        4045
                        4046
                        4059
                        4050
                        4064
                        4065
                        4077
                        4083
                        4087
                        4079
                        4087
                        4082
                        4083
                        4111
                        4112
                        4122
                        4123
                        4126
                        4129
                        4138
                        4139
                        4172
                        4160
                        4135
                        4136
                        4147
                        4182
                        4181
                        4180
                        4184
                        4188
                        4191
                        4194
                        4195
                        4176
                        4185
                        4180
                        4187
                        4188
                        4189
                        4190
                        4211
                        4205
                        4208
                        4220
                        4236
                        4239
                        4240
                        4243
                        4240
                        4269
                        4275
                        4274
                        4272
                        4282
                        4281
                        4286
                        4285
                        4248
                        4247
                        4250
                        4249
                        4244
                        4272
                        4265
                        4269
                        4295
                        4280
                        4279
                        4273
                        4256
                        4245
                        4247
                        4248
                        4228
                        4229
                        4231
                        4245
                        4259
                        4261
                        4268
                        4270
                        4272
                        4276
                        4305
                        4322
                        4325
                        4333
                        4329
                        4330
                        4331
                        4328
                        4332
                        4340
                        4367
                        4363
                        4375
                        4399
                        4408
                        4434
                        4412
                        4420
                        4421
                        4422
                        4433
                        4416
                        4430
                        4434
                        4437
                        4438
                        4445
                        4446
                        4448
                        4450
                        4465
                        4468
                        4482
                        4484
                        4461
                        4462
                        4461
                        4462
                        4464
                        4461
                        4464
                        4465
                        4470
                        4478
                        4480
                        4461
                        4471
                        4464
                        4462
                        4463
                        4464
                        4465
                        4454
                        4457
                        4455
                        4456
                        4467
                        4480
                        4490
                        4491
                        4499
                        4500
                        4517
                        4524
                        4540
                        4551
                        4552
                        4559
                        4560
                        4559
                        4565
                        4558
                        4557
                        4562
                        4568
                        4573
                        4578
                        4557
                        4573
                        4574
                        4572
                        4588
                        4613
                        4614
                        4600
                        4602
                        4601
                        4598
                        4597
                        4601
                        4611
                        4612
                        4613
                        4621
                        4622
                        4645
                        4651
                        4650
                        4663
                        4660
                        4657
                        4660
                        4667
                        4666
                        4669
                        4666
                        4654
                        4657
                        4669
                        4672
                        4651
                        4652
                        4655
                        4653
                        4654
                        4669
                        4670
                        4671
                        4672
                        4673
                        4675
                        4681
                        4682
                        4687
                        4691
                        4692
                        4688
                        4687
                        4713
                        4725
                        4719
                        4720
                        4730
                        4728
                        4725
                        4720
                        4721
                        4722
                        4714
                        4716
                        4724
                        4707
                        4716
                        4707
                        4706
                        4707
                        4708
                        4709
                        4710
                        4711
                        4726
                        4694
                        4701
                        4697
                        4698
                        4678
                        4679
                        4680
                        4670
                        4661
                        4677
                        4678
                        4660
                        4649
                        4650
                        4653
                        4655
                        4650
                        4657
                        4658
                        4660
                        4663
                        4686
                        4691
                        4686
                        4676
                        4658
                        4656
                        4655
                        4657
                        4656
                        4655
                        4670
                        4661
                        4658
                        4659
                        4663
                        4664
                        4673
                        4691
                        4694
                        4696
                        4698
                        4702
                        4706
                        4714
                        4709
                        4707
                        4708
                        4719
                        4725
                        4744
                        4754
                        4782
                        4778
                        4808
                        4826
                        4830
                        4838
                        4841
                        4848
                        4855
                        4866
                        4862
                        4865
                        4852
                        4881
                        4873
                        4875
                        4874
                        4876
                        4869
                        4866
                        4870
                        4867
                        4866
                        4883
                        4912
                        4918
                        4938
                        4960
                        4948
                        4956
                        4961
                        4978
                        4993
                        4994
                        4998
                        5000
                        5001
                        5007
                        5009
                        5015
                        5002
                        5005
                        5039
                        5037
                        5043
                        5044
                        5051
                        5053
                        5057
                        5058
                        5059
                        5058
                        5065
                        5067
                        5066
                        5069
                        5070
                        5077
                        5076
                        5077
                        5078
                        5079
                        5085
                        5081
                        5087
                        5107
                        5122
                        5123
                        5099
                        5066
                        5083
                        5109
                        5105
                        5106
                        5090
                        5112
                        5102
                        5109
                        5108
                        5109
                        5108
                        5109
                        5118
                        5114
                        5117
                        5119
                        5139
                        5144
                        5142
                        5148
                        5151
                        5147
                        5148
                        5160
                        5172
                        5154
                        5156
                        5155
                        5164
                        5155
                        5143
                        5122
                        5147
                        5162
                        5163
                        5151
                        5152
                        5157
                        5163
                        5203
                        5211
                        5222
                        5224
                        5241
                        5239
                        5250
                        5254
                        5250
                        5251
                        5252
                        5256
                        5276
                        5274
                        5246
                        5248
                        5251
                        5250
                        5254
                        5267
                        5285
                        5286
                        5292
                        5290
                        5289
                        5293
                        5294
                        5299
                        5303
                        5305
                        5306
                        5316
                        5317
                        5319
                        5320
                        5326
                        5325
                        5330
                        5329
                        5332
                        5336
                        5338
                        5339
                        5349
                        5353
                        5354
                        5346
                        5348
                        5349
                        5358
                        5359
                        5360
                        5361
                        5377
                        5380
                        5390
                        5391
                        5403
                        5402
                        5405
                        5404
                        5406
                        5405
                        5406
                        5405
                        5406
                        5434
                        5443
                        5444
                        5458
                        5478
                        5484
                        5491
                        5492
                        5504
                        5507
                        5508
                        5514
                        5512
                        5523
                        5529
                        5534
                        5535
                        5558
                        5560
                        5561
                        5534
                        5543
                        5538
                        5537
                        5533
                        5554
                        5558
                        5557
                        5554
                        5590
                        5601
                        5604
                        5623
                        5626
                        5625
                    |]

        let result1 = input |> Array.fold (fun (x,s) y -> (y, if y > x then s+1 else s)) (System.Int32.MaxValue,0)
        let result2 = input |> Array.fold (fun ((x,y,z),s) e -> ((y,z,e),if y + z + e > x + y + z then s+1 else s)) ((999,999,999),0)

    module Ådvent2 =

        let input =  [|
                     "forward 6"
                     "down 2   "
                     "forward 2"
                     "down 8   "
                     "forward 3"
                     "down 6   "
                     "down 8   "
                     "down 9   "
                     "forward 7"
                     "forward 8"
                     "down 9   "
                     "down 8   "
                     "down 9   "
                     "up 8     "
                     "forward 1"
                     "down 7   "
                     "down 3   "
                     "forward 3"
                     "forward 1"
                     "down 3   "
                     "forward 3"
                     "forward 1"
                     "up 8     "
                     "down 5   "
                     "down 1   "
                     "forward 6"
                     "forward 2"
                     "up 9     "
                     "down 3   "
                     "down 8   "
                     "down 3   "
                     "down 3   "
                     "up 2     "
                     "down 7   "
                     "down 3   "
                     "up 5     "
                     "forward 4"
                     "down 9   "
                     "forward 6"
                     "forward 3"
                     "forward 1"
                     "forward 3"
                     "down 2   "
                     "up 9     "
                     "down 4   "
                     "forward 6"
                     "down 3   "
                     "forward 2"
                     "down 2   "
                     "up 5     "
                     "up 1     "
                     "forward 3"
                     "forward 6"
                     "down 6   "
                     "forward 7"
                     "forward 1"
                     "down 3   "
                     "down 8   "
                     "forward 2"
                     "down 7   "
                     "up 1     "
                     "up 2     "
                     "forward 5"
                     "down 8   "
                     "down 8   "
                     "forward 9"
                     "forward 7"
                     "forward 2"
                     "forward 7"
                     "up 6     "
                     "up 9     "
                     "down 4   "
                     "forward 4"
                     "forward 4"
                     "up 1     "
                     "down 7   "
                     "forward 9"
                     "forward 3"
                     "down 6   "
                     "down 9   "
                     "forward 7"
                     "forward 4"
                     "up 7     "
                     "up 6     "
                     "up 8     "
                     "down 9   "
                     "forward 1"
                     "down 1   "
                     "forward 8"
                     "down 7   "
                     "forward 5"
                     "down 3   "
                     "down 3   "
                     "down 8   "
                     "down 8   "
                     "down 4   "
                     "up 4     "
                     "forward 3"
                     "down 8   "
                     "down 9   "
                     "up 3     "
                     "up 8     "
                     "down 9   "
                     "up 5     "
                     "forward 2"
                     "forward 5"
                     "forward 5"
                     "down 8   "
                     "forward 9"
                     "forward 8"
                     "down 5   "
                     "down 9   "
                     "forward 6"
                     "forward 2"
                     "forward 3"
                     "up 1     "
                     "forward 1"
                     "up 2     "
                     "up 2     "
                     "forward 4"
                     "forward 8"
                     "forward 5"
                     "down 1   "
                     "up 4     "
                     "forward 5"
                     "up 7     "
                     "down 5   "
                     "down 5   "
                     "forward 8"
                     "up 2     "
                     "down 7   "
                     "down 6   "
                     "down 5   "
                     "down 5   "
                     "down 1   "
                     "down 8   "
                     "forward 9"
                     "forward 2"
                     "up 6     "
                     "up 4     "
                     "down 8   "
                     "forward 1"
                     "forward 2"
                     "down 2   "
                     "forward 7"
                     "forward 7"
                     "forward 3"
                     "forward 6"
                     "forward 8"
                     "down 3   "
                     "forward 6"
                     "up 5     "
                     "down 3   "
                     "down 8   "
                     "up 1     "
                     "forward 1"
                     "down 7   "
                     "down 3   "
                     "up 5     "
                     "forward 6"
                     "forward 8"
                     "forward 9"
                     "up 5     "
                     "up 5     "
                     "up 5     "
                     "forward 8"
                     "up 5     "
                     "down 6   "
                     "down 7   "
                     "down 5   "
                     "up 7     "
                     "up 1     "
                     "up 3     "
                     "forward 8"
                     "up 9     "
                     "down 7   "
                     "down 4   "
                     "up 6     "
                     "up 8     "
                     "up 9     "
                     "up 9     "
                     "forward 5"
                     "up 5     "
                     "forward 2"
                     "forward 2"
                     "forward 6"
                     "up 2     "
                     "down 8   "
                     "up 2     "
                     "forward 5"
                     "down 9   "
                     "up 7     "
                     "down 9   "
                     "forward 1"
                     "forward 8"
                     "up 1     "
                     "forward 7"
                     "forward 2"
                     "down 3   "
                     "forward 3"
                     "forward 2"
                     "up 9     "
                     "forward 4"
                     "forward 9"
                     "down 9   "
                     "forward 5"
                     "forward 1"
                     "forward 5"
                     "forward 8"
                     "up 5     "
                     "forward 1"
                     "down 4   "
                     "up 8     "
                     "up 4     "
                     "up 7     "
                     "forward 4"
                     "down 1   "
                     "up 6     "
                     "forward 6"
                     "down 2   "
                     "down 7   "
                     "forward 4"
                     "up 7     "
                     "forward 7"
                     "forward 9"
                     "down 5   "
                     "up 5     "
                     "forward 4"
                     "down 6   "
                     "forward 1"
                     "up 8     "
                     "up 8     "
                     "down 8   "
                     "down 7   "
                     "forward 7"
                     "down 3   "
                     "forward 7"
                     "down 3   "
                     "down 5   "
                     "down 4   "
                     "up 8     "
                     "down 2   "
                     "down 2   "
                     "up 5     "
                     "forward 9"
                     "up 9     "
                     "forward 2"
                     "up 4     "
                     "forward 4"
                     "down 2   "
                     "down 7   "
                     "forward 7"
                     "down 1   "
                     "down 6   "
                     "down 4   "
                     "forward 6"
                     "up 4     "
                     "forward 4"
                     "down 6   "
                     "down 8   "
                     "down 3   "
                     "forward 7"
                     "down 3   "
                     "forward 7"
                     "down 7   "
                     "forward 4"
                     "up 9     "
                     "down 5   "
                     "forward 7"
                     "forward 7"
                     "up 6     "
                     "down 3   "
                     "forward 9"
                     "down 1   "
                     "forward 4"
                     "up 9     "
                     "down 3   "
                     "up 9     "
                     "down 5   "
                     "up 6     "
                     "forward 1"
                     "forward 9"
                     "up 4     "
                     "down 3   "
                     "forward 1"
                     "down 7   "
                     "down 2   "
                     "forward 2"
                     "down 6   "
                     "up 4     "
                     "down 4   "
                     "up 9     "
                     "down 3   "
                     "down 9   "
                     "down 4   "
                     "down 1   "
                     "up 8     "
                     "down 2   "
                     "up 1     "
                     "forward 5"
                     "forward 9"
                     "forward 1"
                     "up 4     "
                     "forward 5"
                     "down 7   "
                     "up 6     "
                     "down 3   "
                     "forward 8"
                     "down 1   "
                     "down 5   "
                     "forward 5"
                     "down 5   "
                     "down 7   "
                     "down 8   "
                     "down 7   "
                     "up 6     "
                     "forward 8"
                     "down 8   "
                     "forward 6"
                     "down 6   "
                     "down 7   "
                     "down 3   "
                     "forward 2"
                     "down 6   "
                     "down 8   "
                     "down 7   "
                     "down 3   "
                     "up 1     "
                     "down 7   "
                     "forward 8"
                     "forward 2"
                     "forward 5"
                     "down 4   "
                     "up 4     "
                     "forward 9"
                     "down 9   "
                     "forward 6"
                     "down 7   "
                     "down 4   "
                     "down 8   "
                     "up 9     "
                     "forward 7"
                     "down 4   "
                     "forward 7"
                     "forward 1"
                     "forward 7"
                     "down 9   "
                     "down 7   "
                     "forward 3"
                     "forward 3"
                     "forward 2"
                     "down 5   "
                     "up 5     "
                     "forward 5"
                     "down 2   "
                     "forward 7"
                     "forward 9"
                     "forward 7"
                     "down 7   "
                     "down 9   "
                     "down 5   "
                     "forward 2"
                     "up 5     "
                     "down 3   "
                     "forward 7"
                     "down 4   "
                     "down 3   "
                     "up 5     "
                     "down 6   "
                     "down 3   "
                     "up 4     "
                     "forward 3"
                     "down 1   "
                     "forward 6"
                     "forward 6"
                     "down 8   "
                     "forward 9"
                     "down 2   "
                     "up 3     "
                     "down 4   "
                     "down 5   "
                     "forward 3"
                     "down 9   "
                     "forward 2"
                     "up 3     "
                     "up 4     "
                     "forward 9"
                     "down 2   "
                     "forward 9"
                     "forward 3"
                     "down 4   "
                     "down 2   "
                     "down 5   "
                     "down 4   "
                     "forward 4"
                     "down 1   "
                     "down 9   "
                     "down 2   "
                     "forward 8"
                     "down 5   "
                     "forward 5"
                     "up 7     "
                     "down 5   "
                     "down 2   "
                     "forward 5"
                     "up 4     "
                     "down 5   "
                     "up 3     "
                     "forward 7"
                     "down 9   "
                     "forward 5"
                     "forward 2"
                     "forward 1"
                     "down 7   "
                     "down 9   "
                     "down 2   "
                     "up 2     "
                     "up 2     "
                     "up 4     "
                     "down 4   "
                     "down 7   "
                     "down 3   "
                     "forward 5"
                     "forward 3"
                     "up 6     "
                     "down 6   "
                     "up 6     "
                     "up 9     "
                     "forward 8"
                     "forward 4"
                     "up 3     "
                     "forward 1"
                     "forward 2"
                     "up 5     "
                     "forward 5"
                     "forward 8"
                     "forward 7"
                     "forward 4"
                     "down 1   "
                     "down 8   "
                     "down 1   "
                     "forward 3"
                     "up 1     "
                     "forward 7"
                     "forward 4"
                     "down 8   "
                     "forward 7"
                     "forward 9"
                     "forward 3"
                     "down 9   "
                     "down 9   "
                     "down 3   "
                     "up 6     "
                     "up 1     "
                     "down 4   "
                     "forward 5"
                     "forward 4"
                     "forward 6"
                     "forward 8"
                     "down 6   "
                     "down 3   "
                     "forward 5"
                     "forward 6"
                     "down 4   "
                     "down 2   "
                     "up 3     "
                     "down 3   "
                     "down 7   "
                     "down 5   "
                     "down 5   "
                     "forward 6"
                     "down 4   "
                     "forward 1"
                     "up 2     "
                     "forward 3"
                     "down 1   "
                     "down 4   "
                     "down 9   "
                     "down 7   "
                     "down 9   "
                     "forward 9"
                     "down 6   "
                     "down 3   "
                     "down 2   "
                     "down 5   "
                     "up 8     "
                     "forward 5"
                     "forward 5"
                     "forward 4"
                     "up 5     "
                     "forward 1"
                     "down 9   "
                     "down 1   "
                     "up 5     "
                     "forward 8"
                     "forward 6"
                     "forward 5"
                     "down 1   "
                     "up 5     "
                     "down 8   "
                     "up 7     "
                     "down 8   "
                     "down 2   "
                     "down 3   "
                     "forward 2"
                     "up 4     "
                     "down 6   "
                     "up 6     "
                     "down 3   "
                     "down 7   "
                     "up 3     "
                     "forward 4"
                     "down 3   "
                     "forward 4"
                     "up 9     "
                     "forward 5"
                     "down 2   "
                     "forward 7"
                     "forward 5"
                     "up 3     "
                     "up 2     "
                     "forward 2"
                     "down 8   "
                     "down 1   "
                     "down 3   "
                     "up 5     "
                     "down 4   "
                     "forward 4"
                     "down 1   "
                     "forward 9"
                     "down 3   "
                     "down 7   "
                     "down 4   "
                     "down 4   "
                     "forward 7"
                     "up 5     "
                     "forward 4"
                     "down 8   "
                     "up 4     "
                     "forward 6"
                     "down 1   "
                     "up 4     "
                     "forward 4"
                     "down 6   "
                     "up 5     "
                     "up 1     "
                     "forward 2"
                     "down 5   "
                     "forward 8"
                     "forward 6"
                     "down 8   "
                     "down 7   "
                     "down 7   "
                     "down 1   "
                     "forward 5"
                     "forward 7"
                     "forward 7"
                     "forward 7"
                     "up 3     "
                     "forward 9"
                     "forward 1"
                     "down 9   "
                     "forward 4"
                     "up 8     "
                     "forward 1"
                     "forward 5"
                     "forward 4"
                     "down 2   "
                     "forward 4"
                     "forward 9"
                     "forward 3"
                     "down 1   "
                     "forward 4"
                     "forward 9"
                     "forward 5"
                     "down 5   "
                     "down 5   "
                     "forward 7"
                     "down 3   "
                     "forward 4"
                     "down 6   "
                     "forward 7"
                     "down 2   "
                     "down 1   "
                     "down 5   "
                     "forward 4"
                     "forward 9"
                     "down 4   "
                     "forward 2"
                     "down 8   "
                     "up 5     "
                     "down 9   "
                     "forward 8"
                     "down 3   "
                     "up 6     "
                     "down 2   "
                     "down 4   "
                     "forward 4"
                     "up 2     "
                     "down 4   "
                     "down 4   "
                     "up 7     "
                     "down 6   "
                     "forward 4"
                     "down 7   "
                     "forward 3"
                     "down 1   "
                     "up 1     "
                     "down 2   "
                     "down 6   "
                     "down 4   "
                     "up 3     "
                     "down 6   "
                     "up 2     "
                     "down 6   "
                     "forward 3"
                     "down 9   "
                     "forward 5"
                     "down 5   "
                     "down 9   "
                     "down 9   "
                     "down 7   "
                     "forward 9"
                     "forward 8"
                     "forward 9"
                     "up 9     "
                     "forward 7"
                     "forward 4"
                     "forward 4"
                     "up 5     "
                     "forward 2"
                     "down 1   "
                     "up 9     "
                     "forward 2"
                     "forward 7"
                     "forward 1"
                     "down 9   "
                     "forward 9"
                     "up 8     "
                     "up 1     "
                     "up 7     "
                     "up 7     "
                     "down 5   "
                     "forward 2"
                     "forward 8"
                     "forward 6"
                     "down 7   "
                     "forward 1"
                     "down 9   "
                     "down 4   "
                     "down 4   "
                     "down 1   "
                     "up 7     "
                     "forward 4"
                     "forward 6"
                     "up 5     "
                     "forward 2"
                     "down 9   "
                     "down 7   "
                     "forward 1"
                     "forward 2"
                     "down 5   "
                     "forward 3"
                     "forward 8"
                     "forward 6"
                     "forward 3"
                     "forward 2"
                     "down 1   "
                     "forward 1"
                     "forward 1"
                     "forward 3"
                     "down 9   "
                     "up 9     "
                     "down 9   "
                     "down 6   "
                     "forward 7"
                     "down 6   "
                     "forward 9"
                     "down 9   "
                     "down 7   "
                     "down 1   "
                     "down 9   "
                     "up 9     "
                     "down 6   "
                     "forward 9"
                     "down 6   "
                     "forward 3"
                     "down 8   "
                     "up 5     "
                     "forward 5"
                     "forward 8"
                     "up 3     "
                     "down 8   "
                     "up 6     "
                     "forward 4"
                     "down 2   "
                     "forward 6"
                     "down 9   "
                     "forward 6"
                     "forward 4"
                     "forward 9"
                     "forward 3"
                     "down 2   "
                     "down 4   "
                     "forward 5"
                     "down 9   "
                     "up 7     "
                     "forward 4"
                     "up 1     "
                     "forward 1"
                     "down 6   "
                     "forward 3"
                     "forward 7"
                     "forward 2"
                     "forward 2"
                     "down 5   "
                     "down 9   "
                     "down 3   "
                     "down 5   "
                     "up 3     "
                     "forward 1"
                     "down 2   "
                     "down 4   "
                     "down 1   "
                     "up 9     "
                     "up 5     "
                     "up 1     "
                     "down 1   "
                     "up 9     "
                     "down 5   "
                     "up 3     "
                     "up 3     "
                     "down 7   "
                     "forward 4"
                     "down 6   "
                     "forward 2"
                     "forward 7"
                     "forward 4"
                     "down 2   "
                     "forward 6"
                     "forward 2"
                     "down 3   "
                     "up 3     "
                     "up 9     "
                     "forward 9"
                     "forward 9"
                     "forward 6"
                     "down 8   "
                     "down 1   "
                     "forward 9"
                     "up 1     "
                     "down 6   "
                     "forward 6"
                     "up 5     "
                     "forward 2"
                     "forward 6"
                     "down 9   "
                     "forward 1"
                     "forward 8"
                     "down 8   "
                     "forward 4"
                     "forward 7"
                     "up 6     "
                     "up 1     "
                     "forward 7"
                     "forward 3"
                     "forward 2"
                     "down 4   "
                     "down 7   "
                     "down 7   "
                     "down 1   "
                     "down 6   "
                     "forward 1"
                     "down 9   "
                     "up 9     "
                     "up 9     "
                     "down 2   "
                     "down 2   "
                     "forward 5"
                     "up 2     "
                     "forward 7"
                     "up 5     "
                     "down 9   "
                     "forward 7"
                     "forward 2"
                     "down 8   "
                     "up 1     "
                     "down 5   "
                     "forward 6"
                     "down 8   "
                     "down 7   "
                     "forward 4"
                     "up 2     "
                     "down 8   "
                     "forward 2"
                     "down 5   "
                     "down 4   "
                     "down 9   "
                     "down 1   "
                     "down 9   "
                     "down 6   "
                     "down 3   "
                     "forward 1"
                     "forward 6"
                     "up 1     "
                     "up 1     "
                     "up 9     "
                     "down 2   "
                     "down 2   "
                     "forward 5"
                     "down 3   "
                     "forward 4"
                     "down 3   "
                     "down 7   "
                     "down 7   "
                     "forward 4"
                     "up 3     "
                     "forward 4"
                     "down 3   "
                     "forward 8"
                     "forward 1"
                     "up 2     "
                     "up 1     "
                     "forward 1"
                     "down 6   "
                     "down 1   "
                     "down 3   "
                     "forward 7"
                     "down 7   "
                     "forward 4"
                     "forward 5"
                     "forward 3"
                     "down 5   "
                     "forward 9"
                     "forward 5"
                     "down 7   "
                     "forward 6"
                     "down 4   "
                     "down 4   "
                     "down 9   "
                     "down 3   "
                     "up 9     "
                     "forward 7"
                     "down 7   "
                     "forward 6"
                     "down 2   "
                     "down 9   "
                     "forward 4"
                     "forward 1"
                     "forward 4"
                     "down 5   "
                     "forward 7"
                     "down 9   "
                     "down 8   "
                     "forward 9"
                     "forward 1"
                     "down 9   "
                     "forward 6"
                     "up 5     "
                     "forward 9"
                     "down 1   "
                     "down 5   "
                     "forward 4"
                     "forward 5"
                     "forward 8"
                     "down 5   "
                     "forward 9"
                     "down 6   "
                     "down 2   "
                     "up 4     "
                     "up 8     "
                     "forward 3"
                     "forward 4"
                     "down 3   "
                     "forward 4"
                     "up 6     "
                     "forward 3"
                     "forward 8"
                     "forward 7"
                     "down 1   "
                     "down 9   "
                     "down 8   "
                     "down 8   "
                     "down 1   "
                     "forward 9"
                     "up 4     "
                     "down 5   "
                     "forward 7"
                     "down 8   "
                     "down 3   "
                     "forward 9"
                     "down 5   "
                     "forward 7"
                     "forward 2"
                     "down 4   "
                     "forward 2"
                     "forward 7"
                     "down 6   "
                     "forward 7"
                     "down 2   "
                     "forward 9"
                     "down 9   "
                     "forward 8"
                     "forward 8"
                     "down 6   "
                     "forward 7"
                     "down 8   "
                     "forward 7"
                     "forward 3"
                     "down 1   "
                     "up 8     "
                     "down 5   "
                     "down 6   "
                     "up 5     "
                     "forward 5"
                     "forward 5"
                     "up 5     "
                     "up 3     "
                     "up 7     "
                     "down 6   "
                     "forward 8"
                     "forward 4"
                     "down 2   "
                     "up 5     "
                     "forward 8"
                     "down 6   "
                     "forward 4"
                     "forward 2"
                     "up 8     "
                     "down 8   "
                     "down 5   "
                     "down 4   "
                     "forward 9"
                     "forward 9"
                     "forward 6"
                     "forward 6"
                     "down 3   "
                     "up 1     "
                     "down 4   "
                     "down 8   "
                     "down 9   "
                     "down 1   "
                     "forward 3"
                     "forward 1"
                     "down 9   "
                     "down 3   "
                     "down 7   "
                     "forward 6"
                     "forward 9"
                     "down 8   "
                     "down 8   "
                     "forward 6"
                     "forward 1"
                     "down 3   "
                     "forward 1"
                     "down 8   "
                     "down 3   "
                     "down 9   "
                     "up 1     "
                     "forward 6"
                     "up 2     "
                     "down 3   "
                     "forward 4"
                     "forward 2"
                     "up 2     "
                     "down 5   "
                     "forward 1"
                     "down 3   "
                     "forward 9"
                     "forward 4"
                     "forward 6"
                     "down 3   "
                     "forward 7"
                     "down 6   "
                     "up 3     "
                     "up 7     "
                     "up 5     "
                     "down 4   "
                     "forward 4"
                     "up 1     "
                     "forward 7"
                     "up 9     "
                     "forward 3"
                     "up 1     "
                     "down 3   "
                     "down 4   "
                     "forward 4"
                     "up 3     "
                     "down 6   "
                     "down 9   "
                     "down 6   "
                     "forward 4"
                     "down 9   "
                     "down 6   "
                     "forward 4"
                     "forward 3"
                     "down 3   "
                     "up 7     "
                     "down 9   "
                     "forward 8"
                     |]

        let parse (position : int * int) (command : string) : int * int =
            if command.StartsWith "forward" then
                let x = (command.Replace("forward","")) |> (fun (s : string) -> s.Trim()) |> int
                (fst position, (snd position) + x)
            else if command.StartsWith "down" then
                let x = (command.Replace("down","")) |> (fun (s : string) -> s.Trim()) |> int
                ((fst position + x), (snd position))
            else
                let x = (command.Replace("up","")) |> (fun (s : string) -> s.Trim()) |> int
                ((fst position) - x, (snd position))

        let result1 = input |> Array.fold parse (0,0) |> (fun t -> fst t * snd t)


        type Submarine =
            {
                Position : int
                Depth : int
                Aim : int
            }

        let move (sub : Submarine) (command : string) : Submarine =
            if command.StartsWith "forward" then
                let x = (command.Replace("forward","")) |> (fun (s : string) -> s.Trim()) |> int
                {sub with Position = sub.Position + x; Depth = sub.Depth + sub.Aim * x}
            else if command.StartsWith "down" then
                let x = (command.Replace("down","")) |> (fun (s : string) -> s.Trim()) |> int
                {sub with Aim = sub.Aim + x}
            else
                let x = (command.Replace("up","")) |> (fun (s : string) -> s.Trim()) |> int
                {sub with Aim = sub.Aim - x}

        let result2 = input |> Array.fold move {Position = 0; Depth = 0 ; Aim = 0} |> (fun s -> s.Position * s.Depth)

    module Advent3 =

        let input = [|
                        "111110110111"
                        "100111000111"
                        "011101111101"
                        "011011010010"
                        "001010001010"
                        "111101001001"
                        "010001110011"
                        "100001001111"
                        "110101010000"
                        "100001100010"
                        "000011000100"
                        "111101000101"
                        "100100100110"
                        "100000011111"
                        "101101111101"
                        "100000011110"
                        "001110101111"
                        "101011101011"
                        "110011000001"
                        "010100011110"
                        "100011000011"
                        "001000101001"
                        "101110010110"
                        "101110010100"
                        "110000001101"
                        "010001010101"
                        "110000010010"
                        "111011101100"
                        "101111011100"
                        "101000010111"
                        "011000011001"
                        "110011110010"
                        "110100011010"
                        "110110010011"
                        "001110001100"
                        "011011001100"
                        "000011101111"
                        "111010101001"
                        "000101001000"
                        "000010111111"
                        "110100100111"
                        "011000101000"
                        "101011001101"
                        "000110110110"
                        "010101001111"
                        "010011010110"
                        "101111100001"
                        "000111101100"
                        "000000101110"
                        "101110001111"
                        "010000001101"
                        "011111010011"
                        "101100011111"
                        "011011000000"
                        "110000010111"
                        "001011010010"
                        "101111100000"
                        "101010000010"
                        "011101101010"
                        "110010000111"
                        "011110011111"
                        "110000010110"
                        "011110101000"
                        "111001011000"
                        "111010011100"
                        "011111000101"
                        "011111110100"
                        "111101101010"
                        "111101101001"
                        "011011110011"
                        "100110100010"
                        "100000110000"
                        "010001010011"
                        "100110111000"
                        "101010110100"
                        "011011011010"
                        "010100100000"
                        "011110011100"
                        "110101000001"
                        "001010111101"
                        "011110011000"
                        "001101101001"
                        "010011001110"
                        "100000100111"
                        "000100101011"
                        "110000000010"
                        "100011101011"
                        "100101010010"
                        "001010100111"
                        "001111011100"
                        "010111100101"
                        "000111000000"
                        "010001011101"
                        "100111001001"
                        "111100010101"
                        "000110011000"
                        "000010101001"
                        "110001110101"
                        "011101000110"
                        "011101111011"
                        "110100011101"
                        "011001100010"
                        "011011010100"
                        "001110111101"
                        "000100110001"
                        "100100000000"
                        "010001110010"
                        "110010010100"
                        "010100111011"
                        "111010010100"
                        "010010111010"
                        "011001100000"
                        "110101110111"
                        "001111100101"
                        "001011100000"
                        "010011001000"
                        "010011011100"
                        "000000111001"
                        "010101110000"
                        "001100011100"
                        "000010110011"
                        "111000101010"
                        "100000010010"
                        "001001011101"
                        "011100100001"
                        "110111101000"
                        "101110001001"
                        "001011000110"
                        "100110011010"
                        "101010000111"
                        "110110000110"
                        "001100101101"
                        "000000001011"
                        "110001110011"
                        "010001100111"
                        "010100011111"
                        "000000010001"
                        "001100110011"
                        "011010011110"
                        "101100111101"
                        "000111000110"
                        "101000111010"
                        "011011001011"
                        "011111000000"
                        "111011010110"
                        "000011001111"
                        "101100100000"
                        "100111010000"
                        "010000001010"
                        "001010001011"
                        "000011000010"
                        "111001110100"
                        "000011000011"
                        "000001101001"
                        "000000010010"
                        "100100010001"
                        "011010010111"
                        "110111011010"
                        "101101001110"
                        "001110010110"
                        "010100110001"
                        "101101010100"
                        "000000111100"
                        "110011111110"
                        "000010100101"
                        "100111010111"
                        "110111101111"
                        "111010011000"
                        "110000111100"
                        "101001011100"
                        "101000100101"
                        "100010001110"
                        "101111001100"
                        "101100111000"
                        "000100000110"
                        "001100100100"
                        "101011011011"
                        "111010010101"
                        "100011100101"
                        "001100000111"
                        "100111000001"
                        "010001011010"
                        "101101110100"
                        "100011011110"
                        "110001110000"
                        "111100101000"
                        "100111111011"
                        "110000101010"
                        "101100110100"
                        "001001000001"
                        "110100011100"
                        "010111110001"
                        "110111101010"
                        "100001110010"
                        "100000000011"
                        "000111100001"
                        "111010011011"
                        "010101011110"
                        "001000001101"
                        "010111010101"
                        "100100011000"
                        "111000110010"
                        "110001110100"
                        "111001101000"
                        "110000111110"
                        "010111001100"
                        "011101110011"
                        "101001101011"
                        "011011110000"
                        "000111010010"
                        "110101010111"
                        "011101000101"
                        "000100100101"
                        "011100011101"
                        "010000100100"
                        "000101111011"
                        "110111010010"
                        "110111100010"
                        "000110111100"
                        "011011010011"
                        "111111111011"
                        "001111000101"
                        "111110010110"
                        "010000011011"
                        "111110011101"
                        "100111101100"
                        "001000100011"
                        "111010000010"
                        "001001001100"
                        "010000010111"
                        "101010100011"
                        "111110010000"
                        "110100100001"
                        "001111000010"
                        "110010000000"
                        "111110110001"
                        "100001101110"
                        "101011010101"
                        "111100100011"
                        "001111000000"
                        "101111111011"
                        "001001010101"
                        "001110111000"
                        "101010001001"
                        "110111000000"
                        "010101110011"
                        "001110101110"
                        "000011010101"
                        "000110111011"
                        "000010001000"
                        "100000100011"
                        "001001001001"
                        "011101111111"
                        "100101100101"
                        "010011101110"
                        "101000101100"
                        "110011000101"
                        "001101111000"
                        "000001011000"
                        "010100100110"
                        "111011001000"
                        "110110010111"
                        "001000001011"
                        "111000101001"
                        "011010011100"
                        "111011010011"
                        "011001101111"
                        "011000111111"
                        "010100000000"
                        "000111010101"
                        "101111011010"
                        "000100011111"
                        "011000000100"
                        "101111110010"
                        "111110001100"
                        "101111111000"
                        "111000001101"
                        "100000001101"
                        "100011110001"
                        "100111110100"
                        "010100000111"
                        "000000010000"
                        "010110011001"
                        "111001100001"
                        "110001011011"
                        "010101100011"
                        "110011011010"
                        "110001011000"
                        "110111010011"
                        "101000011110"
                        "111100011111"
                        "101011000111"
                        "101000011001"
                        "000100000000"
                        "010111000011"
                        "110001000100"
                        "001010011010"
                        "010010011000"
                        "110100001000"
                        "010000101010"
                        "111110000001"
                        "110101000000"
                        "000001110101"
                        "000010101101"
                        "010100001111"
                        "010110100010"
                        "011111110111"
                        "111000001000"
                        "000110001010"
                        "000010100011"
                        "011110101001"
                        "000101000100"
                        "100101110011"
                        "111110011100"
                        "011101001010"
                        "001001001110"
                        "110101111111"
                        "000101011100"
                        "001100110111"
                        "010010000010"
                        "000111010111"
                        "111100001110"
                        "011110110011"
                        "110110000010"
                        "101110011111"
                        "100101011001"
                        "111001111111"
                        "111010110110"
                        "110110011000"
                        "110110111010"
                        "100100111010"
                        "001010101011"
                        "100011001000"
                        "101011101010"
                        "111100111110"
                        "100010101000"
                        "011011111001"
                        "010111111110"
                        "111111011010"
                        "100111110000"
                        "100100100011"
                        "011101011100"
                        "011101011011"
                        "010001000110"
                        "011100010100"
                        "110111110000"
                        "110110101101"
                        "110001001001"
                        "110101011011"
                        "101101100011"
                        "101101010000"
                        "111011001111"
                        "100111100100"
                        "100110000000"
                        "100011011010"
                        "010100000001"
                        "001100011101"
                        "010011100011"
                        "000101011111"
                        "000111010011"
                        "001100110101"
                        "011011001110"
                        "101111000100"
                        "111110111101"
                        "111111000100"
                        "001101111101"
                        "001111111111"
                        "110101001101"
                        "011111110001"
                        "111001010001"
                        "001000110001"
                        "010010110111"
                        "000111111111"
                        "000001011010"
                        "100101101110"
                        "100001111100"
                        "110110010000"
                        "110110101100"
                        "010110101011"
                        "011111101010"
                        "001110101010"
                        "001111001101"
                        "000001101010"
                        "000100111100"
                        "101111100100"
                        "000010011110"
                        "100110010100"
                        "111101101110"
                        "100011111101"
                        "100111100001"
                        "011110100101"
                        "111101100110"
                        "101001111010"
                        "101001001010"
                        "001000101111"
                        "001111001011"
                        "101000100011"
                        "110100100110"
                        "110010101111"
                        "000001000001"
                        "001011010111"
                        "111101011011"
                        "000100000101"
                        "111001001101"
                        "110001101001"
                        "010111000001"
                        "101110101110"
                        "110100010011"
                        "001011110000"
                        "101000001011"
                        "011110111101"
                        "001010011111"
                        "001101110101"
                        "100001110110"
                        "111111111111"
                        "010011011011"
                        "000111101000"
                        "011011011000"
                        "000000001101"
                        "101111100101"
                        "000101101010"
                        "010111001000"
                        "011110110100"
                        "110100110010"
                        "000110011110"
                        "001011110100"
                        "001111101010"
                        "110100111101"
                        "010101011011"
                        "010101101011"
                        "101011100011"
                        "011110011010"
                        "111111001111"
                        "001100110010"
                        "101111101111"
                        "110010001001"
                        "101101100101"
                        "000111000101"
                        "000101010001"
                        "011100111110"
                        "001000001001"
                        "101100100010"
                        "001110010000"
                        "111110000010"
                        "110110001010"
                        "010001010110"
                        "001101010011"
                        "100011010111"
                        "100011000001"
                        "001111011011"
                        "001100001110"
                        "000110001100"
                        "000001101000"
                        "111101010111"
                        "010011100101"
                        "010010111001"
                        "011100111001"
                        "001000000111"
                        "010001000100"
                        "000110010000"
                        "000100011110"
                        "110110101111"
                        "100111111110"
                        "011111110101"
                        "100101100000"
                        "110101100011"
                        "010100100100"
                        "110011110100"
                        "001001101010"
                        "111110100111"
                        "000010101000"
                        "001010011001"
                        "101111100011"
                        "010110100001"
                        "100101001111"
                        "001010010010"
                        "101010101110"
                        "101101101010"
                        "011100010011"
                        "100001001101"
                        "001010011100"
                        "000010111101"
                        "001110100011"
                        "101000110000"
                        "111011001101"
                        "100100001010"
                        "111011111000"
                        "001100111111"
                        "010001111000"
                        "001010101111"
                        "101001110001"
                        "010111111101"
                        "010000011101"
                        "000011101010"
                        "001100111100"
                        "100110001110"
                        "001101001010"
                        "000001100001"
                        "011001001111"
                        "101011000011"
                        "110111101101"
                        "111101101100"
                        "001010110001"
                        "110101000110"
                        "010111010110"
                        "110100111010"
                        "101010011101"
                        "001000101110"
                        "111111011101"
                        "111100011100"
                        "010101110101"
                        "011100011011"
                        "111101100000"
                        "011100101111"
                        "110110100001"
                        "101010010111"
                        "100101101111"
                        "000001001000"
                        "111001000110"
                        "001101110001"
                        "100001000101"
                        "110110011100"
                        "100101110100"
                        "111010101110"
                        "100111110101"
                        "000010101011"
                        "011100110110"
                        "111110100110"
                        "111100000111"
                        "101111010111"
                        "100001000011"
                        "001011000000"
                        "100010010101"
                        "111000101101"
                        "101010100010"
                        "111110111000"
                        "000101000001"
                        "101001110110"
                        "110001101010"
                        "100011100111"
                        "100111001101"
                        "001100001001"
                        "010111110011"
                        "000011111101"
                        "010011100100"
                        "110101110011"
                        "101110101100"
                        "111100111000"
                        "101100011000"
                        "010101010000"
                        "001110000000"
                        "000000001100"
                        "010011111010"
                        "100101010100"
                        "010011110000"
                        "100011110000"
                        "000110011101"
                        "111111110010"
                        "100111100000"
                        "100001011110"
                        "111011000110"
                        "111001011011"
                        "110110100111"
                        "100101110001"
                        "111010100001"
                        "101101011000"
                        "100000100000"
                        "011000100110"
                        "100000100010"
                        "100010011100"
                        "001000011100"
                        "111111010010"
                        "000011011101"
                        "011001011110"
                        "000101100010"
                        "000100010000"
                        "110100111100"
                        "111011011001"
                        "100011010101"
                        "110010000100"
                        "001101100101"
                        "001001110100"
                        "101011011111"
                        "010000111000"
                        "111011100101"
                        "000010000100"
                        "010011110100"
                        "111000010101"
                        "000011111010"
                        "111100000001"
                        "011110010010"
                        "101111101100"
                        "000101011001"
                        "100110101100"
                        "000111110011"
                        "101110000011"
                        "011000101100"
                        "010001101011"
                        "111101000100"
                        "001010000110"
                        "111101111100"
                        "101000110001"
                        "010011010011"
                        "011111100111"
                        "110001011010"
                        "111110110011"
                        "110001011100"
                        "110010001011"
                        "110101101100"
                        "001001011110"
                        "110000010101"
                        "001100000010"
                        "111001010111"
                        "101001010010"
                        "101011101100"
                        "000110101011"
                        "101101101011"
                        "111100111011"
                        "111111000001"
                        "110100101010"
                        "010010110101"
                        "111101010101"
                        "000011100011"
                        "111000100100"
                        "000101001100"
                        "111100001100"
                        "110010001111"
                        "110010011001"
                        "110011111011"
                        "100101110010"
                        "100010010110"
                        "111001001000"
                        "010010010110"
                        "001000000110"
                        "111110011010"
                        "010100001100"
                        "111011011000"
                        "111011001110"
                        "100111010001"
                        "100110101000"
                        "000010100001"
                        "101111101001"
                        "100001100100"
                        "100000100100"
                        "010000100000"
                        "011111111000"
                        "111011111101"
                        "000001011001"
                        "000001011101"
                        "011000011111"
                        "111101111000"
                        "110000011110"
                        "011100110001"
                        "101001000001"
                        "111000111110"
                        "110111111101"
                        "101010001011"
                        "110101011001"
                        "001111100000"
                        "100100110000"
                        "100111000101"
                        "111011101111"
                        "010000010011"
                        "100101100111"
                        "100000110111"
                        "010101011000"
                        "111110110101"
                        "010000101111"
                        "110011001100"
                        "011010011011"
                        "000001100100"
                        "101110110110"
                        "010000101011"
                        "110101100001"
                        "000001011100"
                        "000100011101"
                        "100001000100"
                        "011110100001"
                        "010111001010"
                        "011000110111"
                        "011010001000"
                        "011000111001"
                        "010000010000"
                        "010001101100"
                        "010110111010"
                        "000000010110"
                        "101101111001"
                        "110000111111"
                        "111110010100"
                        "110110100010"
                        "101100001001"
                        "001000010011"
                        "011101101001"
                        "010111011010"
                        "001110110001"
                        "011110110111"
                        "111101100100"
                        "101011010111"
                        "011111011101"
                        "010000010101"
                        "111101001011"
                        "000110001000"
                        "010110101110"
                        "101001100100"
                        "101010010010"
                        "110010010000"
                        "101101001011"
                        "100110111001"
                        "010000000001"
                        "000111001011"
                        "011011101000"
                        "110110010100"
                        "110011111001"
                        "101110111010"
                        "111010001010"
                        "000010110010"
                        "000011000111"
                        "101000100111"
                        "101000000100"
                        "111110100000"
                        "110100110011"
                        "011100010000"
                        "100000001010"
                        "100000111101"
                        "111011010000"
                        "000001110001"
                        "111011000000"
                        "110100100100"
                        "010001001101"
                        "011100100100"
                        "011000101001"
                        "101011010110"
                        "001011001101"
                        "110100001101"
                        "011110001101"
                        "001010101110"
                        "011001000100"
                        "000011111000"
                        "111100001011"
                        "001111011001"
                        "010100000110"
                        "111001101110"
                        "001100011000"
                        "100001101001"
                        "101000101111"
                        "101100111011"
                        "100011111111"
                        "011100000100"
                        "000000001000"
                        "100100100010"
                        "111000110001"
                        "001100111110"
                        "001110011100"
                        "100010010111"
                        "100001000010"
                        "000111110100"
                        "010001010010"
                        "110111001011"
                        "010111110101"
                        "010110000010"
                        "110100010111"
                        "011000001110"
                        "011011011100"
                        "110010011010"
                        "101010101101"
                        "100111011111"
                        "111111101101"
                        "000010001001"
                        "000000110001"
                        "000100111011"
                        "111100110101"
                        "001011000100"
                        "111111110100"
                        "011001010111"
                        "000000000000"
                        "011100000011"
                        "000001000100"
                        "101000011100"
                        "101010011001"
                        "101011111000"
                        "011111001010"
                        "111110010101"
                        "001010110110"
                        "001011110110"
                        "110110110001"
                        "110100111111"
                        "000100110000"
                        "101000100001"
                        "001101000011"
                        "111110011110"
                        "000101101100"
                        "000011111011"
                        "100101100110"
                        "011101010111"
                        "101101000100"
                        "110000101110"
                        "110111111000"
                        "101111001011"
                        "111001101010"
                        "000010000110"
                        "001011011001"
                        "011010110010"
                        "010001110100"
                        "001011010000"
                        "101101000011"
                        "111111010100"
                        "011001110101"
                        "011011111000"
                        "010101010100"
                        "001000101100"
                        "111000000010"
                        "100000111011"
                        "100000000100"
                        "000000000110"
                        "100100111110"
                        "011110000110"
                        "100111001100"
                        "111010111000"
                        "001011010011"
                        "101100001101"
                        "110000001010"
                        "110101111100"
                        "101010111011"
                        "001100111101"
                        "101100111010"
                        "101010000011"
                        "011111001100"
                        "100011111110"
                        "010010011110"
                        "110001110110"
                        "010011011000"
                        "001001010100"
                        "101110001011"
                        "000101110101"
                        "101000000011"
                        "001011011110"
                        "101010000100"
                        "111111111001"
                        "101111001010"
                        "011010100101"
                        "011001001011"
                        "110110110101"
                        "010000111011"
                        "100001010011"
                        "010111100000"
                        "011000010100"
                        "001010011110"
                        "100101011101"
                        "001010000011"
                        "111011010010"
                        "000101111001"
                        "011011001000"
                        "101000010011"
                        "000111111001"
                        "110111010111"
                        "001100001011"
                        "001111001010"
                        "001110100110"
                        "101010011100"
                        "001100010110"
                        "101110111100"
                        "010110000011"
                        "000100101001"
                        "111111000010"
                        "000010110110"
                        "001111000110"
                        "010001011110"
                        "000101001010"
                        "011001100110"
                        "101010100000"
                        "010110110111"
                        "100010110010"
                        "101100001100"
                        "110010000001"
                        "101111101011"
                        "011100011001"
                        "010101000000"
                        "101010111010"
                        "110101110010"
                        "111101100001"
                        "111000100110"
                        "110101001001"
                        "110100110100"
                        "001110100010"
                        "001011011011"
                        "111011100110"
                        "010100111000"
                        "100100100111"
                        "011000000111"
                        "101110111000"
                        "011110000011"
                        "010011101011"
                        "101000001001"
                        "011101100100"
                        "000110110000"
                        "010100001010"
                        "011110010011"
                        "101010100110"
                        "101100110000"
                        "011110000100"
                        "111000010111"
                        "100100001000"
                        "000110001001"
                        "010000101100"
                        "010111101100"
                        "110101100100"
                        "100000111001"
                        "010101101001"
                        "101000001100"
                        "010100110011"
                        "100101000111"
                        "101001111111"
                        "000110011100"
                        "000101000110"
                        "001101000000"
                        "010101011101"
                        "110111011100"
                        "111010010110"
                        "111111000101"
                        "100111000110"
                        "100011001100"
                        "011000110100"
                        "011011011001"
                        "001000010111"
                        "100011101110"
                        "110011101111"
                        "001000110010"
                        "110010101110"
                        "001110011110"
                        "010110110100"
                        "000010011000"
                        "011111101000"
                        "100000001110"
                        "110100101110"
                        "001110110111"
                        "100110111010"
                        "101110101011"
                        "000110000001"
                        "010111110100"
                        "011111111011"
                        "110001100011"
                        "000101100100"
                        "100110011110"
                        "011000101010"
                        "011111100001"
                        "000000111110"
                        "010010110110"
                        "110101111110"
                        "101110011011"
                        "101001011000"
                        "000101010011"
                        "011110100000"
                        "001100010100"
                        "000011010010"
                        "001110100111"
                        "011010000000"
                        "001011100010"
                        "111100011000"
                        "100100100001"
                        "101010110101"
                        "100101010111"
                        "100010010100"
                        "111011011110"
                        "100001010101"
                        "011111111110"
                        "001011001001"
                        "100111010101"
                        "001000011010"
                        "100110111101"
                        "001111011110"
                        "101011100010"
                        "010110001111"
                        "100000101110"
                        "001000000011"
                        "111101100010"
                        "100110110011"
                        "100011001011"
                        "010101110110"
                        "000111111000"
                        "011110111111"
                        "100000010001"
                        "011001101101"
                        "100111001011"
                        "000010000111"
                        "110000101011"
                        "010000110101"
                        "101010111111"
                        "111110001110"
                        "111101110001"
                        "100100100000"
                        "000001110110"
                        "001101010000"
                        "111011100010"
                        "110001111110"
                        "100011001110"
                        "001001100001"
                        "000111111100"
                        "000100100000"
                        "000000101001"
                        "101011000001"
                    |]

        //let input = [|
        //                "00100"
        //                "11110"
        //                "10110"
        //                "10111"
        //                "10101"
        //                "01111"
        //                "00111"
        //                "11100"
        //                "10000"
        //                "11001"
        //                "00010"
        //                "01010"
        //            |]

        let indices = [0 .. (input |> Array.head |> Seq.length) - 1]

        let countOnes i =
            input
            |> Array.map (fun (s : string) -> s.Substring(i,1))
            |> Array.filter ((=) "1")
            |> Array.length

        let a = indices
                |> List.map countOnes
                |> List.map ((<) (input.Length / 2))
                |> List.map (fun b -> if b then "1" else "0")
                |> String.concat ""
                |> (fun b -> System.Convert.ToInt32(b,2))

        let func (inp : string []) i =
            if inp.Length = 1 then
                inp
            else
                let ones = inp
                           |> Array.map (fun (s : string) -> s.Substring(i,1))
                           |> Array.filter ((=) "1")
                           |> Array.length

                let majority = if decimal ones >= (decimal inp.Length / 2m) then "1" else "0"

                inp
                |> Array.filter (fun (s : string) -> s.Substring(i,1) <> majority)

        let b = indices
                |> List.fold func input
                |> Array.exactlyOne
                |> (fun b -> System.Convert.ToInt32(b,2))

    module Advent4 =

        type BoardNumber =
            {
                Number : int
                Marked : bool
                Board : int
                Row : int
                Column : int
            }

        let toBoardnumber index n : BoardNumber =
            let i = float index
            let board = i / 25.0 |> floor
            {
            Number = n
            Marked = false
            Board = board |> int
            Row = (i / 5.0 |> floor) - board * 5.0 |> int
            Column = i % 5.0 |> int
            }

        let inputNumbers = "6,69,28,50,36,84,49,13,48,90,1,33,71,0,94,59,53,58,60,96,30,34,29,91,11,41,77,95,17,80,85,93,7,9,74,89,18,25,26,8,87,38,68,5,12,43,27,46,62,73,16,55,22,4,65,76,54,52,83,10,21,67,15,47,45,40,35,66,79,51,75,39,64,24,37,72,3,44,82,32,78,63,57,2,86,31,19,92,14,97,20,56,88,81,70,61,42,99,23,98".Split [|','|]
                           |> Array.map int

        let inputBoards = System.IO.File.ReadAllLines(@"C:\Users\STP\Desktop\testinput.txt")
                          |> Array.filter (fun s -> s = "" |> not)
                          |> Array.collect (fun s -> s.Split [|' '|])
                          |> Array.filter (fun s -> s = "" |> not)
                          |> Array.map int
                          |> Array.mapi toBoardnumber

        open System.Collections.Generic
        let winners = new List<int * int>()

        let advanceBoards (preBoards : BoardNumber []) (number : int) : BoardNumber [] =
            let newBoards = preBoards
                            |> Array.map (fun n -> if n.Number=number then {n with Marked = true} else n)

            let checkBoard iter (bn : int) : unit =
                let board = newBoards |> Array.filter (fun b -> b.Board=bn)

                let checkRows row = board |> Array.filter (fun b -> b.Row = row && b.Marked = true) |> Array.length |> ((=) 5)
                let checkColums row = board |> Array.filter (fun b -> b.Column = row && b.Marked = true) |> Array.length |> ((=) 5)

                let unmarkedSum = board |> Array.filter (fun n -> n.Marked = false) |> Array.sumBy (fun b -> b.Number)

                [|0 .. 4|]
                |> Array.iter (fun i -> if checkRows i then winners.Add(bn,(number * unmarkedSum)) else ())

                [|0 .. 4|]
                |> Array.iter (fun i -> if checkColums i then winners.Add(bn,(number * unmarkedSum)) else ())

            [|0 .. (inputBoards |> Array.last).Board|]
            |> Array.iteri checkBoard

            newBoards


        let orderedWinners = inputNumbers
                             |> Array.fold advanceBoards inputBoards

        let kek = winners |> Seq.distinctBy fst

        printfn "%A" 5


    module Ådvent5 =
        //let input = [|
        //                "456,846 -> 221,846"
        //                "980,926 -> 73,19  "
        //                "682,930 -> 562,930"
        //                "766,592 -> 274,100"
        //                "247,685 -> 247,21 "
        //                "106,800 -> 635,800"
        //                "953,340 -> 135,340"
        //                "293,223 -> 293,12 "
        //                "454,196 -> 454,463"
        //                "886,766 -> 164,766"
        //                "592,590 -> 192,590"
        //                "436,982 -> 436,545"
        //                "731,571 -> 420,260"
        //                "741,11 -> 466,11  "
        //                "727,541 -> 579,541"
        //                "341,553 -> 25,553 "
        //                "942,470 -> 942,196"
        //                "203,600 -> 203,647"
        //                "965,595 -> 949,611"
        //                "554,306 -> 554,401"
        //                "902,438 -> 902,728"
        //                "864,609 -> 525,270"
        //                "187,790 -> 187,323"
        //                "956,950 -> 427,950"
        //                "847,554 -> 422,554"
        //                "935,900 -> 701,900"
        //                "192,854 -> 866,180"
        //                "512,946 -> 543,915"
        //                "978,979 -> 491,979"
        //                "708,61 -> 708,878 "
        //                "738,508 -> 282,52 "
        //                "23,25 -> 841,843  "
        //                "204,750 -> 204,797"
        //                "703,500 -> 703,419"
        //                "14,311 -> 694,311 "
        //                "646,301 -> 785,301"
        //                "397,168 -> 439,168"
        //                "680,931 -> 561,812"
        //                "540,448 -> 90,448 "
        //                "706,668 -> 91,53  "
        //                "848,319 -> 318,319"
        //                "198,948 -> 198,307"
        //                "686,58 -> 686,541 "
        //                "867,234 -> 867,498"
        //                "134,125 -> 134,688"
        //                "824,566 -> 53,566 "
        //                "437,167 -> 276,167"
        //                "94,65 -> 638,609  "
        //                "36,971 -> 971,36  "
        //                "494,330 -> 494,197"
        //                "920,438 -> 920,364"
        //                "698,84 -> 49,733  "
        //                "59,842 -> 59,876  "
        //                "328,577 -> 328,677"
        //                "757,701 -> 134,78 "
        //                "466,274 -> 135,605"
        //                "81,925 -> 988,18  "
        //                "40,142 -> 882,984 "
        //                "50,96 -> 882,928  "
        //                "782,47 -> 782,427 "
        //                "247,599 -> 24,599 "
        //                "112,812 -> 191,733"
        //                "487,198 -> 144,198"
        //                "327,663 -> 327,756"
        //                "117,76 -> 688,76  "
        //                "530,71 -> 530,958 "
        //                "558,602 -> 671,489"
        //                "677,830 -> 677,556"
        //                "529,669 -> 349,669"
        //                "336,966 -> 341,971"
        //                "20,31 -> 851,862  "
        //                "423,880 -> 423,573"
        //                "521,657 -> 552,657"
        //                "412,822 -> 18,428 "
        //                "423,311 -> 423,105"
        //                "381,614 -> 705,614"
        //                "521,248 -> 394,121"
        //                "286,47 -> 286,403 "
        //                "286,27 -> 711,452 "
        //                "347,61 -> 489,61  "
        //                "760,454 -> 760,954"
        //                "746,573 -> 911,573"
        //                "839,933 -> 839,776"
        //                "124,815 -> 290,649"
        //                "577,848 -> 419,848"
        //                "393,206 -> 410,206"
        //                "364,755 -> 881,755"
        //                "788,68 -> 788,215 "
        //                "94,798 -> 192,798 "
        //                "292,250 -> 453,250"
        //                "601,545 -> 293,237"
        //                "438,923 -> 438,655"
        //                "70,757 -> 887,757 "
        //                "184,402 -> 818,402"
        //                "586,49 -> 103,49  "
        //                "202,315 -> 735,315"
        //                "534,504 -> 534,523"
        //                "367,236 -> 367,736"
        //                "24,163 -> 24,240  "
        //                "185,426 -> 634,875"
        //                "485,189 -> 39,189 "
        //                "556,30 -> 374,30  "
        //                "969,821 -> 676,528"
        //                "254,435 -> 254,43 "
        //                "290,615 -> 741,164"
        //                "345,601 -> 120,826"
        //                "224,641 -> 887,641"
        //                "190,716 -> 581,325"
        //                "552,646 -> 552,393"
        //                "413,177 -> 413,103"
        //                "397,900 -> 360,900"
        //                "138,980 -> 138,55 "
        //                "909,891 -> 909,593"
        //                "926,986 -> 79,139 "
        //                "954,67 -> 53,968  "
        //                "180,30 -> 595,30  "
        //                "823,165 -> 823,660"
        //                "285,176 -> 375,176"
        //                "915,826 -> 184,95 "
        //                "735,230 -> 667,230"
        //                "934,865 -> 917,865"
        //                "48,602 -> 737,602 "
        //                "477,319 -> 385,411"
        //                "981,17 -> 11,987  "
        //                "458,401 -> 24,401 "
        //                "118,415 -> 849,415"
        //                "176,678 -> 176,852"
        //                "567,753 -> 567,37 "
        //                "285,868 -> 830,323"
        //                "555,623 -> 822,623"
        //                "522,546 -> 674,546"
        //                "880,21 -> 23,878  "
        //                "591,103 -> 591,407"
        //                "434,64 -> 434,401 "
        //                "245,968 -> 275,968"
        //                "726,510 -> 450,786"
        //                "768,366 -> 768,738"
        //                "488,745 -> 488,94 "
        //                "675,674 -> 675,705"
        //                "618,237 -> 265,237"
        //                "802,709 -> 802,59 "
        //                "144,696 -> 144,542"
        //                "547,381 -> 547,799"
        //                "78,667 -> 78,916  "
        //                "409,271 -> 302,271"
        //                "294,694 -> 938,50 "
        //                "140,571 -> 97,571 "
        //                "682,875 -> 682,534"
        //                "748,816 -> 748,183"
        //                "84,622 -> 84,258  "
        //                "485,696 -> 582,599"
        //                "909,233 -> 954,233"
        //                "203,711 -> 203,350"
        //                "335,904 -> 455,904"
        //                "578,778 -> 578,21 "
        //                "830,954 -> 902,954"
        //                "78,252 -> 78,682  "
        //                "920,220 -> 684,220"
        //                "309,301 -> 104,301"
        //                "270,795 -> 270,919"
        //                "906,479 -> 304,479"
        //                "627,164 -> 627,986"
        //                "122,960 -> 915,167"
        //                "664,916 -> 770,810"
        //                "692,810 -> 826,810"
        //                "981,951 -> 192,162"
        //                "183,423 -> 809,423"
        //                "632,464 -> 567,464"
        //                "94,266 -> 94,587  "
        //                "261,770 -> 569,770"
        //                "51,403 -> 466,818 "
        //                "631,645 -> 187,645"
        //                "141,238 -> 141,145"
        //                "357,21 -> 173,21  "
        //                "138,248 -> 839,949"
        //                "889,957 -> 807,957"
        //                "399,431 -> 105,725"
        //                "548,331 -> 548,821"
        //                "790,844 -> 43,97  "
        //                "675,671 -> 221,671"
        //                "874,143 -> 620,397"
        //                "205,435 -> 205,546"
        //                "521,434 -> 822,133"
        //                "141,86 -> 257,86  "
        //                "427,28 -> 290,165 "
        //                "49,694 -> 567,694 "
        //                "846,344 -> 266,924"
        //                "425,910 -> 433,918"
        //                "956,498 -> 485,27 "
        //                "798,498 -> 798,634"
        //                "879,13 -> 766,126 "
        //                "737,475 -> 737,425"
        //                "338,473 -> 425,386"
        //                "510,615 -> 214,319"
        //                "758,415 -> 758,490"
        //                "969,208 -> 239,938"
        //                "917,188 -> 917,528"
        //                "34,820 -> 806,820 "
        //                "85,633 -> 857,633 "
        //                "262,355 -> 262,748"
        //                "373,784 -> 971,186"
        //                "146,577 -> 60,663 "
        //                "613,570 -> 613,199"
        //                "300,319 -> 300,108"
        //                "764,171 -> 764,17 "
        //                "555,921 -> 555,825"
        //                "241,197 -> 770,197"
        //                "600,832 -> 600,807"
        //                "934,987 -> 20,73  "
        //                "960,730 -> 837,730"
        //                "976,50 -> 46,980  "
        //                "829,834 -> 153,158"
        //                "785,835 -> 785,58 "
        //                "586,633 -> 689,736"
        //                "804,250 -> 348,706"
        //                "226,539 -> 16,539 "
        //                "411,940 -> 98,940 "
        //                "289,589 -> 893,589"
        //                "738,616 -> 738,55 "
        //                "225,54 -> 542,54  "
        //                "793,246 -> 303,736"
        //                "332,752 -> 984,100"
        //                "413,18 -> 839,444 "
        //                "840,122 -> 840,233"
        //                "989,970 -> 215,196"
        //                "329,361 -> 573,605"
        //                "242,537 -> 242,619"
        //                "943,898 -> 943,535"
        //                "469,865 -> 501,833"
        //                "226,717 -> 196,687"
        //                "819,803 -> 712,803"
        //                "532,663 -> 532,672"
        //                "61,931 -> 940,52  "
        //                "623,218 -> 274,567"
        //                "281,326 -> 281,790"
        //                "815,176 -> 679,176"
        //                "790,862 -> 942,710"
        //                "18,771 -> 18,514  "
        //                "479,377 -> 309,377"
        //                "704,402 -> 704,150"
        //                "961,335 -> 492,335"
        //                "745,829 -> 745,477"
        //                "556,543 -> 771,543"
        //                "832,336 -> 917,251"
        //                "742,755 -> 742,174"
        //                "206,735 -> 493,735"
        //                "151,216 -> 312,55 "
        //                "445,157 -> 615,157"
        //                "781,143 -> 781,76 "
        //                "833,717 -> 514,398"
        //                "357,14 -> 357,36  "
        //                "771,405 -> 771,422"
        //                "662,886 -> 169,886"
        //                "689,990 -> 22,990 "
        //                "680,445 -> 379,445"
        //                "92,369 -> 502,779 "
        //                "64,948 -> 64,363  "
        //                "295,957 -> 976,276"
        //                "113,920 -> 634,399"
        //                "542,662 -> 305,899"
        //                "566,514 -> 566,645"
        //                "528,106 -> 549,106"
        //                "205,367 -> 821,367"
        //                "313,105 -> 313,928"
        //                "532,177 -> 532,664"
        //                "862,773 -> 905,816"
        //                "800,796 -> 911,796"
        //                "870,80 -> 11,939  "
        //                "188,900 -> 154,900"
        //                "420,509 -> 520,609"
        //                "540,863 -> 28,863 "
        //                "31,72 -> 78,72    "
        //                "823,648 -> 503,648"
        //                "879,252 -> 606,252"
        //                "677,117 -> 677,507"
        //                "743,303 -> 196,850"
        //                "220,491 -> 220,891"
        //                "216,815 -> 577,815"
        //                "540,819 -> 745,819"
        //                "152,721 -> 382,721"
        //                "280,745 -> 985,745"
        //                "479,367 -> 358,488"
        //                "913,413 -> 649,413"
        //                "40,678 -> 817,678 "
        //                "467,533 -> 467,214"
        //                "132,68 -> 843,779 "
        //                "519,109 -> 669,259"
        //                "619,791 -> 221,791"
        //                "114,622 -> 628,622"
        //                "951,636 -> 866,636"
        //                "172,569 -> 775,569"
        //                "244,972 -> 173,972"
        //                "283,64 -> 739,520 "
        //                "68,604 -> 68,156  "
        //                "529,30 -> 529,925 "
        //                "813,883 -> 137,883"
        //                "893,231 -> 629,231"
        //                "673,658 -> 673,389"
        //                "725,899 -> 218,899"
        //                "317,318 -> 105,318"
        //                "82,706 -> 100,688 "
        //                "222,227 -> 440,227"
        //                "810,371 -> 810,985"
        //                "414,321 -> 289,446"
        //                "901,158 -> 260,799"
        //                "198,967 -> 717,448"
        //                "928,454 -> 875,454"
        //                "974,437 -> 974,764"
        //                "657,13 -> 760,13  "
        //                "498,966 -> 976,966"
        //                "66,104 -> 66,15   "
        //                "773,569 -> 980,362"
        //                "420,496 -> 403,513"
        //                "57,920 -> 85,920  "
        //                "879,551 -> 879,662"
        //                "98,395 -> 98,398  "
        //                "483,685 -> 483,55 "
        //                "222,935 -> 586,935"
        //                "89,926 -> 807,208 "
        //                "744,160 -> 744,462"
        //                "588,973 -> 588,548"
        //                "312,572 -> 38,298 "
        //                "27,131 -> 552,656 "
        //                "591,935 -> 591,86 "
        //                "907,478 -> 907,279"
        //                "981,75 -> 981,972 "
        //                "316,947 -> 935,947"
        //                "906,38 -> 906,216 "
        //                "374,521 -> 345,550"
        //                "579,29 -> 579,107 "
        //                "444,636 -> 444,557"
        //                "458,608 -> 830,980"
        //                "479,839 -> 155,515"
        //                "766,600 -> 766,71 "
        //                "976,965 -> 31,20  "
        //                "928,49 -> 269,708 "
        //                "787,238 -> 787,983"
        //                "583,742 -> 112,742"
        //                "966,268 -> 554,680"
        //                "671,354 -> 671,966"
        //                "274,340 -> 274,894"
        //                "673,185 -> 607,185"
        //                "73,171 -> 874,171 "
        //                "861,526 -> 861,410"
        //                "739,591 -> 739,138"
        //                "209,355 -> 209,146"
        //                "286,501 -> 887,501"
        //                "495,902 -> 700,902"
        //                "192,889 -> 821,260"
        //                "400,21 -> 154,21  "
        //                "861,301 -> 325,301"
        //                "552,990 -> 511,990"
        //                "908,21 -> 11,918  "
        //                "127,724 -> 821,30 "
        //                "935,46 -> 170,811 "
        //                "947,91 -> 374,91  "
        //                "625,420 -> 265,60 "
        //                "214,228 -> 546,228"
        //                "375,547 -> 715,887"
        //                "516,350 -> 870,350"
        //                "610,138 -> 665,193"
        //                "214,621 -> 678,621"
        //                "497,248 -> 600,145"
        //                "549,558 -> 576,558"
        //                "364,537 -> 364,312"
        //                "840,324 -> 310,854"
        //                "441,945 -> 441,458"
        //                "459,531 -> 459,100"
        //                "937,113 -> 150,900"
        //                "277,405 -> 259,405"
        //                "409,527 -> 409,359"
        //                "534,766 -> 534,740"
        //                "534,934 -> 681,934"
        //                "456,419 -> 83,419 "
        //                "871,986 -> 873,986"
        //                "14,59 -> 916,961  "
        //                "911,963 -> 971,963"
        //                "25,325 -> 139,211 "
        //                "937,184 -> 354,767"
        //                "460,416 -> 289,245"
        //                "193,171 -> 861,839"
        //                "840,299 -> 840,911"
        //                "531,45 -> 531,619 "
        //                "599,315 -> 455,315"
        //                "455,97 -> 455,811 "
        //                "38,748 -> 392,748 "
        //                "841,79 -> 841,88  "
        //                "105,571 -> 105,545"
        //                "801,458 -> 344,458"
        //                "491,535 -> 558,535"
        //                "835,814 -> 223,202"
        //                "563,85 -> 405,85  "
        //                "410,396 -> 600,396"
        //                "273,670 -> 818,125"
        //                "671,647 -> 817,647"
        //                "46,892 -> 678,260 "
        //                "456,736 -> 110,736"
        //                "962,941 -> 619,598"
        //                "388,406 -> 53,71  "
        //                "558,895 -> 227,564"
        //                "944,182 -> 807,319"
        //                "484,898 -> 59,473 "
        //                "808,214 -> 488,534"
        //                "451,679 -> 155,383"
        //                "858,931 -> 381,931"
        //                "723,377 -> 723,281"
        //                "694,283 -> 182,795"
        //                "385,191 -> 320,256"
        //                "33,380 -> 584,931 "
        //                "480,91 -> 817,91  "
        //                "677,91 -> 677,126 "
        //                "291,651 -> 760,182"
        //                "832,962 -> 153,283"
        //                "38,60 -> 479,501  "
        //                "249,350 -> 789,350"
        //                "603,341 -> 266,678"
        //                "52,303 -> 52,102  "
        //                "911,201 -> 559,201"
        //                "46,210 -> 46,275  "
        //                "960,212 -> 554,212"
        //                "375,374 -> 169,580"
        //                "10,10 -> 989,989  "
        //                "844,140 -> 40,944 "
        //                "916,408 -> 916,815"
        //                "834,401 -> 834,169"
        //                "553,479 -> 784,248"
        //                "543,452 -> 543,848"
        //                "854,910 -> 334,390"
        //                "685,491 -> 793,491"
        //                "552,943 -> 709,943"
        //                "723,367 -> 124,367"
        //                "95,55 -> 881,841  "
        //                "155,267 -> 573,267"
        //                "59,357 -> 84,357  "
        //                "218,435 -> 218,344"
        //                "491,584 -> 491,649"
        //                "676,445 -> 676,333"
        //                "361,618 -> 783,618"
        //                "220,295 -> 220,267"
        //                "668,758 -> 299,389"
        //                "965,845 -> 674,845"
        //                "285,603 -> 47,603 "
        //                "853,417 -> 853,757"
        //                "859,906 -> 856,906"
        //                "55,364 -> 753,364 "
        //                "893,474 -> 978,474"
        //                "602,32 -> 58,576  "
        //                "171,445 -> 96,370 "
        //                "214,592 -> 214,286"
        //                "400,946 -> 745,946"
        //                "559,37 -> 112,484 "
        //                "624,510 -> 90,510 "
        //                "329,714 -> 329,850"
        //                "458,287 -> 657,287"
        //                "99,385 -> 99,949  "
        //                "50,736 -> 719,67  "
        //                "273,195 -> 273,306"
        //                "490,902 -> 490,798"
        //                "619,131 -> 921,131"
        //                "266,652 -> 266,730"
        //                "745,661 -> 745,555"
        //                "311,878 -> 311,679"
        //                "491,982 -> 643,830"
        //                "735,875 -> 816,875"
        //                "936,353 -> 936,529"
        //                "792,467 -> 565,467"
        //                "141,140 -> 141,988"
        //                "98,171 -> 414,487 "
        //                "257,259 -> 257,484"
        //                "24,41 -> 969,986  "
        //                "302,453 -> 223,453"
        //                "807,363 -> 492,678"
        //                "823,22 -> 835,10  "
        //                "301,94 -> 399,94  "
        //                "946,110 -> 248,808"
        //                "983,985 -> 21,23  "
        //                "510,145 -> 510,58 "
        //                "13,661 -> 13,639  "
        //                "218,260 -> 218,54 "
        //                "475,846 -> 475,770"
        //                "458,644 -> 458,529"
        //                "912,934 -> 912,136"
        //                "152,823 -> 550,823"
        //                "136,470 -> 443,470"
        //                "253,871 -> 905,219"
        //                "765,212 -> 793,240"
        //                "11,402 -> 11,42   "
        //                "348,813 -> 348,768"
        //                "368,321 -> 823,776"
        //                "343,495 -> 343,809"
        //                "117,616 -> 117,273"
        //                "92,92 -> 732,92   "
        //                "914,31 -> 28,917  "
        //                "259,944 -> 214,944"
        //                "630,759 -> 462,759"
        //                "134,653 -> 134,610"
        //                "14,989 -> 988,15  "
        //                "139,181 -> 139,451"
        //                "598,636 -> 598,442"
        //                "263,42 -> 686,465 "
        //            |]

        let input = System.IO.File.ReadAllLines @"C:\Users\STP\Desktop\testinput.txt"

        let a=5

        let toCoord (s : string) =
            let coords = s.Split [|','|]
            (int coords.[0]),(int coords.[1])

        let addPoint (state : Map<int*int,int>) (p : int * int) : Map<int*int,int> =
            state.Change(p,(fun x -> match x with | None -> Some 1 | Some d -> d + 1 |> Some))

        let inputToPoints (state : Map<int*int,int>) (inp : string) =
            let split = inp.Split [|' '|]
            let fra = split.[0] |> toCoord
            let til = split.[2] |> toCoord
            let points = match fra, til with
                         | f, t when fst f = fst t -> (if snd f <= snd t then [snd f .. snd t] else [snd t .. snd f]) |> List.map (fun u -> fst f,u)
                         | f, t when snd f = snd t -> (if fst f <= fst t then [fst f .. fst t] else [fst t .. fst f]) |> List.map (fun u -> u,snd f)
                         | _ -> let length = fst fra - fst til |> abs |> (+) 1
                                let travel = List.init length id
                                match fra, til with
                                | f, t when fst f <= fst t && snd f <= snd t -> travel |> List.map (fun i -> fst f + i, snd f + i)
                                | f, t when fst f > fst t && snd f < snd t -> travel |> List.map (fun i -> fst f - i, snd f + i)
                                | f, t when fst f < fst t && snd f > snd t -> travel |> List.map (fun i -> fst f + i, snd f - i)
                                | f, t when fst f > fst t && snd f > snd t -> travel |> List.map (fun i -> fst f - i, snd f - i)
                                | _ -> failwith ""

            points |> List.fold addPoint state

        let play = input
                   |> Array.fold inputToPoints Map.empty
                   |> Map.toList
                   |> List.filter (fun e -> snd e > 1)
                   |> List.length


    module Ådvent6 =
        let testInput = System.IO.File.ReadAllLines @"H:\AdventOfCode\fullinput6.txt"

        let startState = testInput
                         |> Array.head
                         |> (fun s -> s.Split(','))
                         |> Array.map int

        let advanceState state (d : decimal) =
            state
            |> Array.collect (fun i -> if i = 0 then [|6;8|] else [|i-1|])

        //let result = [|1 .. 80|] |> Array.fold advanceState startState
        //                         |> Array.length



        let start = [|0 .. 8|]
                    |> Array.map (fun i -> startState |> Array.filter (fun x -> x=i) |> Array.length |> decimal)

        let takeStep (state : decimal []) d =
            [|state.[1];state.[2];state.[3];state.[4];state.[5];state.[6];  state.[7]+state.[0]  ;state.[8];state.[0]|]

        let result2 = [|1 .. 256|]
                      |> Array.fold takeStep start
                      |> Array.sum

        let a = System.Decimal.MaxValue
        let b = 5


    module ÅÅdvent7 =
        let testInput = System.IO.File.ReadAllLines @"H:\AdventOfCode\fullinput7.txt"
                        |> Array.head
                        |> (fun s -> s.Split(','))
                        |> Array.map int

        let calculateFuelUsage position =
            testInput
            |> Array.map ((-) position >> abs)
            |> Array.map (fun n -> n * (n + 1) / 2)
            |> Array.sum

        let result = [|testInput |> Array.min .. testInput |> Array.max|]
                     |> Array.map calculateFuelUsage
                     |> Array.min

        let a = 5

    module Advent8 =
        let input = System.IO.File.ReadAllLines @"H:\AdventOfCode\fullinput8.txt"
                    |> Array.map (fun s -> s.Split('|'))
                    |> Array.map (fun s -> s.[0].Split(' ') |> Array.filter (fun s -> s = "" |> not),s.[1].Split(' ') |> Array.filter (fun s -> s = "" |> not))

        let result = input
                     |> Array.map snd
                     |> Array.collect id
                     |> Array.sumBy (fun s -> match s.Length with |2|3|4|7 -> 1 | _ -> 0)

        let a = 5

    module Ådvent9 =

        type Point = {
                        X : int
                        Y : int
                        Height : int
                        isLowPoint : bool
                        Basin : Point option
                     }

        let input = System.IO.File.ReadAllLines @"H:\AdventOfCode\fullinput9.txt"
                    |> Array.map Array.ofSeq
                    |> array2D
                    |> Array2D.map ((>>) string int)
                    |> Array2D.mapi (fun x y h -> {X = x; Y = y; Height = h ; isLowPoint = false; Basin = None})
                    |> Seq.cast<Point>

        let findNeighbors (p : Point) : Point seq =
            input
            |> Seq.cast<Point>
            |> Seq.filter (fun q -> (p.X - q.X |> abs) + (p.Y - q.Y |> abs) = 1)

        //let Points = input
        //             |> Array2D.map (fun p -> if findNeighbors p |> Seq.map (fun q -> q.Height) |> Seq.min > p.Height then {p with isLowPoint = true} else p)
        //             |> Seq.cast<Point>
        //             |> Seq.filter (fun p -> p.isLowPoint)
        //             |> Seq.sumBy (fun p -> p.Height + 1)

        let rec advance x =
            let stepdown = input |> Seq.tryFind (fun q -> (x.X - q.X |> abs) + (x.Y - q.Y |> abs) = 1 && q.Height < x.Height)
            if stepdown.IsNone then x else advance stepdown.Value

        let findBasin (p : Point) : Point =
            if p.Height = 9 then
                p
            else
                {p with Basin = advance p |> Some}

        let Basins = input
                     |> Seq.map findBasin
                     |> Seq.groupBy (fun p -> p.Basin)
                     |> Seq.map (fun b -> snd b |> Seq.length)
                     |> Seq.sortDescending
                     |> Seq.take 4
                     |> Seq.iter (printfn "%A")


        let a = 5

    module Advent10 =
        0

    module Ådvent11 =

        type Squid = {
                        X : int
                        Y : int
                        Energy : int
                        Flashes : int
                        hasFlashed : bool
                     }

        let input = System.IO.File.ReadAllLines @"H:\AdventOfCode\fullinput11.txt"
                    |> Array.map Array.ofSeq
                    |> array2D
                    |> Array2D.map ((>>) string int)
                    |> Array2D.mapi (fun x y h -> {X = x ; Y = y ; Energy = h ; Flashes = 0 ; hasFlashed = false})
                    |> Seq.cast<Squid>

        let increase s =
            {s with Energy = s.Energy + 1}

        let flash (squid : Squid) (state: Squid seq) : Squid seq =
            state
            |> Seq.map (fun s -> if s.X = squid.X && s.Y = squid.Y then
                                     {s with Flashes = s.Flashes + 1 ; hasFlashed = true}
                                 else if s.X - squid.X |> abs = 1 && s.Y - squid.Y |> abs = 1 then
                                    increase s
                                 else if s.X = squid.X && s.Y - squid.Y |> abs = 1 then
                                    increase s
                                 else if s.Y = squid.Y && s.X - squid.X |> abs = 1 then
                                    increase s
                                 else
                                    s
                       )

        let rec advance (step : int) (state : Squid seq) =
            let squid = state |> Seq.tryFind (fun s -> s.Energy > 9 && s.hasFlashed = false)

            if squid.IsSome then
                flash squid.Value state |> advance step
            else
                let endstate = state
                               |> Seq.map (fun s -> if s.hasFlashed = true then {s with hasFlashed = false ; Energy = 0} else s)
                if state |> Seq.forall (fun s -> s.hasFlashed) then sprintf "%i" step |> failwith
                endstate

        let takeStep (state : Squid seq) (step : int) =
            let incrState = Seq.map increase state

            advance step incrState

        let resultState = [1 .. 500]
                          |> Seq.fold takeStep input

        let result = resultState
                     |> Seq.sumBy (fun s -> s.Flashes)

        let a = 5

module Advent2022 =
    5

module PiratWhist =
    open System

    type Kulør = | Spar
                 | Klør
                 | Ruder
                 | Hjerter

    type Kort = {Værdi : int
                 Kulør : Kulør}

    let choose n k = List.fold (fun s i -> s * (n-i+1)/i ) 1 [1..k] |> decimal

    let n = 3 // Modspillere
    let mitUdspil = true
    let mitKort = {Værdi = 2
                   Kulør = Klør}


    let calcWinProb (kort : Kort) udspil =
        match kort.Kulør, udspil with
        | Spar, _ ->
            let m = 14 - kort.Værdi
            (choose (51 - m) n) / (choose 51 n)
        | _, true ->
            let m = 13 + 14 - kort.Værdi
            (choose (51 - m) n) / (choose 51 n)
        | _, false ->
            let overlevUdspillet =  (decimal (kort.Værdi - 2)) / 51m
            let m = 13 + 14 - kort.Værdi
            overlevUdspillet * (choose (50 - m) (n-1)) / (choose 50 (n-1))



    let hasWon (mykort : Kort) udspil (kort : Kort list) =
        match mykort.Kulør, udspil with
        | Spar, _ ->
            kort |> List.exists (fun k -> k.Kulør = Spar && k.Værdi > mykort.Værdi) |> not
        | _, true ->
            kort |> List.exists (fun k -> k.Kulør = Spar || k.Værdi > mykort.Værdi) |> not
        | _, false ->
            let kulør = kort.Head.Kulør
            if mykort.Kulør <> kulør
            then false
            else kort |> List.exists (fun k -> k.Kulør = kulør && k.Værdi > mykort.Værdi) |> not

    //let intToCard i =
    //    let kulør = match i / 13 with
    //                | 0 -> Hjerter
    //                | 1 -> Ruder
    //                | 2 -> Klør
    //                | 3 -> Spar
    //    let værdi = i % 13
    //    {Værdi = værdi + 2
    //     Kulør = kulør}

    let kortSpil = [Hjerter;Ruder;Klør;Spar] |> List.map (fun kulør -> [2 .. 14] |> List.map (fun i -> {Værdi = i; Kulør = kulør}))
                                             |> List.collect id

    let cardsInPlay = kortSpil |> List.filter ((<>) mitKort)

    let shuffleR (r : Random) xs = xs |> List.sortBy (fun _ -> r.Next())

    let playRounds numberOfRounds =
        let cardsInPlay = kortSpil |> List.filter ((<>) mitKort)
        let cardsDealt i = cardsInPlay |> shuffleR (Random ()) |> List.take n
        [1 .. numberOfRounds]
        |> List.map (fun i -> cardsDealt i |> hasWon mitKort mitUdspil)


    let a = calcWinProb mitKort mitUdspil
    let b = playRounds 1000000 |> List.averageBy (fun b -> if b then 1m else 0m)




    let inline plusTo x =
        (float x) + 2.0

    let temp = plusTo 5m
    let temp2 = plusTo 5.0
