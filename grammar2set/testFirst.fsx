open Maskiner
open System.Collections.Generic
open System.Linq
let fopen name =
    let l = System.IO.File.ReadAllLines ("tests/" + name)
    Array.fold
        (fun acc line -> acc + line + "\n")
        ""
        l
let set2str (set) =
    let retstr =
        Set<GrammarParser.ProdExp>.fold
            (fun acc e ->
                match e with
                | GrammarParser.Term t ->
                    let t =
                        if t = "{" || t = "}" then
                            "'" + t + "'"
                        else t
                    acc + "," + t
                | GrammarParser.Dollar -> acc + ",$"
                | _ -> acc
                )
            ""
            set
    if String.length retstr = 0 then "{}"
    else
        let retstr0 = retstr.[1..]
        "{" + retstr0 + "}"
let calcFirst str =
    let (parsed,pOrder) = GrammarParser.parseGrammar str
    let nullableS = Grammar2Set.calcNullable parsed
    (Grammar2Set.calcFirst parsed nullableS,pOrder)
let printFirst name =
    let str = fopen name
    let (n,order) = calcFirst str
    printfn "Grammar %s:\n%s\nFirstSet:" name str 
    for k in order do
        printfn "First(%s) = %s" k (set2str n.[k])
    printfn "----"

printFirst "grammar1"
printFirst "grammar2"
printFirst "grammar3"
printFirst "grammar4"
printFirst "grammar5"
printFirst "grammar6"
printFirst "grammar7"
printFirst "grammar8"
printFirst "grammar9"
printFirst "grammar10"
