open Maskiner
open GrammarParser
open Grammar2Slr
let fopen name =
    let l = System.IO.File.ReadAllLines ("tests/" + name)
    Array.fold
        (fun acc line -> acc + line + "\n")
        ""
        l
let grammar1 = fopen "test1"
let parsed1 = parseGrammar grammar1
let nfa1 = createTables parsed1

printfn "grammar1 : %A" nfa1
