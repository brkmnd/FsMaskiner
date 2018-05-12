open Maskiner
open System.Collections.Generic
open System.Linq
let fopen name =
    let l = System.IO.File.ReadAllLines ("tests/" + name)
    Array.fold
        (fun acc line -> acc + line + "\n")
        ""
        l
let calcNullable str =
    let (parsed,pOrder) = GrammarParser.parseGrammar str
    Grammar2Set.calcNullable parsed
let printNullable name =
    let str = fopen name
    let n = calcNullable str
    let l = n.Keys.ToList()
    l.Sort()
    printfn "NullableS for %s:\n%s\n" name str 
    for k in l do
        printfn "Nullable(%s) = %A" k n.[k]
    printfn "----"

printNullable "grammar1"
printNullable "grammar2"
printNullable "grammar3"
printNullable "grammar4"
printNullable "grammar5"
printNullable "grammar6"
printNullable "grammar7"
printNullable "grammar8"
printNullable "grammar9"
printNullable "grammar10"
//printNullable "grammarFail1"
