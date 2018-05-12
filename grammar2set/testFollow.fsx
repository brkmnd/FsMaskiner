open Maskiner
open System.Collections.Generic
open System.Linq
let fopen name =
    let l = System.IO.File.ReadAllLines ("tests/" + name)
    Array.fold
        (fun acc line -> acc + line + "\n")
        ""
        l

let calcFollow str =
    let (parsed,pOrder) = GrammarParser.parseGrammar str
    let nullableS = Grammar2Set.calcNullable parsed
    let firstS = Grammar2Set.calcFirst parsed nullableS
    (Grammar2Set.calcFollow parsed firstS nullableS,pOrder)
let set2str set =
    let retstr =
        Set<Grammar2Set.ProdExp>.fold
            (fun acc e ->
                match e with
                | Grammar2Set.Term t ->
                    let t =
                        if t = "{" || t = "}" then
                            "'" + t + "'"
                        else t
                    acc + "," + t
                | Grammar2Set.Dollar -> acc + ",$"
                | _ -> acc
                )
            ""
            set
    if String.length retstr = 0 then "{}"
    else
        let retstr0 = retstr.[1..]
        "{" + retstr0 + "}"
let printFollow name =
    let str = fopen name
    let (n,order) = calcFollow str
    printfn "FollowSet for %s:\n%s\n" name str 
    for k in order do
        printfn "Follow(%s) = %s" k (set2str n.[k])
    printfn "----"

printFollow "grammar1"
printFollow "grammar2"
printFollow "grammar3"
printFollow "grammar4"
printFollow "grammar5"
printFollow "grammar6"
printFollow "grammar7"
printFollow "grammar8"
printFollow "grammar9"
printFollow "grammar10"
