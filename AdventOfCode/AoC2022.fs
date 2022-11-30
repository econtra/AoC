module AoC2022

open NUnit.Framework
open System
open System.Diagnostics
open System.IO

[<TestFixture>]
type ``TestPrint`` () =

    let testfun x = x + 2

    [<Test>]
    member _.``Kør TestPrint`` () =
        let poo = 2

        Assert.Pass()