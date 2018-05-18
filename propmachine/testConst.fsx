open Maskiner
open PropParser
open Prop2Table
open System.Collections.Generic
let fopen name =
    let l = System.IO.File.ReadAllLines ("tests/" + name)
    Array.fold
        (fun acc line -> acc + line + "\n")
        ""
        l
let delimline = "<<<<<<<<<<<<<<<<<<<<<<<>>>>>>>>>>>>>>>>>>>>>>"
let sval s =
    if s = "T" then "\u22A4" 
    else "\u22A5"
let mutable resstr = ""
let existingAtomList = new List<string>()
let echoSit args =
    let argsL = Array.length args
    existingAtomList.Clear()
    resstr <- resstr + (args.[1]|>sval) + "\n"
    true
let echoAtom args =
    let argsL = Array.length args
    if args.[0] = "var" && not (existingAtomList.Contains(args.[1])) then
        existingAtomList.Add(args.[1])
        resstr <- resstr + (args.[2]|>sval) + " "
        true
    else true
let echoes = new Dictionary<string,System.Func<string [],bool>>()
echoes.Add("model",new System.Func<string [],bool>(echoSit))
echoes.Add("atom",new System.Func<string [],bool>(echoAtom))
let printTest fName =
    resstr <- ""
    printfn "%s" delimline
    printfn "test (%s)" fName
    let input = fopen fName
    let (vtab,tree) =
        try parse input with
         | Failure msg ->
             let stdColor = System.Console.ForegroundColor
             System.Console.ForegroundColor <- System.ConsoleColor.Red
             printfn "%s" msg
             System.Console.ForegroundColor <- stdColor 
             ([],EmptyTree)
    let atoms =
        match tree with
        | EmptyTree -> [||]
        | _ -> eval vtab tree echoes
    printfn "expression: %s" input.[0 .. input.Length - 2]
    printfn "table:"
    for atom in atoms do
        printf "%s " atom
    printfn "exp"
    printf "%s" resstr

printTest "test1"
printTest "test2"
printTest "test3"
printTest "test4"
printTest "test5"
printTest "test6"
printTest "test7"
printTest "test8"
//connectives
printfn "<<<<<<<<<<<<testing connectives>>>>>>>>>>>>"
printfn "<<<<<<<<<<<<testing connectives>>>>>>>>>>>>"
printfn "<<<<<<<<<<<<testing connectives>>>>>>>>>>>>"
printfn "<<<<<<<<<<<<testing connectives>>>>>>>>>>>>"
printTest "connectives/atom"
printTest "connectives/not"
printTest "connectives/imp"
printTest "connectives/biimp"
printTest "connectives/or"
printTest "connectives/and"
//tautoligies
printfn "<<<<<<<<<<<<testing tauts>>>>>>>>>>>>"
printfn "<<<<<<<<<<<<testing tauts>>>>>>>>>>>>"
printfn "<<<<<<<<<<<<testing tauts>>>>>>>>>>>>"
printfn "<<<<<<<<<<<<testing tauts>>>>>>>>>>>>"
printTest "tauts/ad_absurdum"
printTest "tauts/demorgans1"
printTest "tauts/demorgans2"
printTest "tauts/imp"
printTest "tauts/modus_ponendo_tollens"
printTest "tauts/modus_tollendo_ponens"
printTest "tauts/modus_ponendo_tollens"
printTest "tauts/modus_tollens"
printTest "tauts/not_imp"
//used in the rapport
printTest "rapport/one"
