open Maskiner
open MathParser
open System.Collections.Generic
let fopen name =
    let l = System.IO.File.ReadAllLines ("tests_parser/" + name)
    Array.fold
        (fun acc line -> acc + line + "\n")
        ""
        l
let factF (args : float[]) =
    let n = (int) args.[0]
    let lookup = [|1.000000;1.000000;2.000000;6.000000;24.000000;120.000000;720.000000;5040.000000;40320.000000;362880.000000;3628800.000000;39916800.000000;479001600.000000|]
    let rec exec acc = function
        | 0 -> acc
        | n -> exec (n * acc) (n - 1)
    if n < lookup.Length then
        lookup.[n]
    else
        float (exec 1 n)
let binomF (args : float[]) =
    let n = (int) args.[0]
    let k = (int) args.[1]
    let rec nChooseK = function
        | (n,k) when k > n -> 0
        | (n,k) when k = 0 -> 1
        | (n,k) when k > n / 2 ->
            printfn "binom hertil"
            nChooseK (n,n-k)
        | (n,k) -> n * (nChooseK (n-1,k-1)) / k
    (float) (nChooseK (n,k))
let dpmf_binomF (args : float[]) =
    let k = args.[0]
    let n = args.[1]
    let p = args.[2]
    (binomF [|n;k|]) * System.Math.Pow(p,k) * System.Math.Pow(1.0 - p,n - k)
let sqrtF (args : float[]) =
    let arg = args.[0]
    System.Math.Sqrt arg
let minusF (args : float[]) =
    let arg1 = args.[0]
    let arg2 = args.[1]
    arg1 - arg2
let expF (args :float[]) =
    let argsN = Array.length args
    if argsN = 0 then
        System.Math.E
    else
        let arg = args.[0]
        System.Math.Exp(arg)
let piF (args : float[]) =
    let argsN = Array.length args
    if argsN = 0 then
        System.Math.PI
    else
        let arg = args.[0]
        System.Math.Pow(System.Math.PI,arg)
let newFunc (f : float[] -> float) =
    new System.Func<float[],float>(f)
let ftab = new Dictionary<string,int[] * System.Func<float[],float>>()
ftab.Add("sqrt",([|1|],newFunc sqrtF))
ftab.Add("minus",([|2|],newFunc minusF))
ftab.Add("exp",([|0;1|],newFunc expF))
ftab.Add("pi",([|0;1|],newFunc piF))
ftab.Add("factorial",([|1|],newFunc factF))
ftab.Add("binomial",([|2|],newFunc binomF))
ftab.Add("dpmf.binomial",([|3|],newFunc dpmf_binomF))
let printTest fName =
    printfn "test (%s)" fName
    let input = fopen fName
    let tree =
        try parse input ftab with
         | Failure msg ->
             let stdColor = System.Console.ForegroundColor
             System.Console.ForegroundColor <- System.ConsoleColor.Red
             printfn "%s" msg
             System.Console.ForegroundColor <- stdColor 
             [EmptyTree]
    printfn "input: %s" input
    printfn "tree: %A" tree
    printfn ""

printTest "test1"
printTest "test2"
printTest "test3"
printTest "test4"
printTest "test5"
printTest "test6"
printTest "test7"
printTest "test8"
printTest "test9"
printTest "test10"
printTest "test11"
printTest "test12"
printTest "test13"
printTest "test14"
printTest "test15"

printfn "errors"
printTest "syntax_error/test1"
printTest "syntax_error/test2"
printTest "syntax_error/test3"
printTest "syntax_error/test4"
printTest "syntax_error/test5"
printTest "syntax_error/test6"
printTest "syntax_error/test7"
printTest "syntax_error/test8"
printTest "syntax_error/test9"
