open Maskiner
let fopen name =
    let l = System.IO.File.ReadAllLines (name)
    Array.fold
        (fun acc line -> acc + line + "\n")
        ""
        l
let parseGrammar name =
    let str = fopen name
    GrammarParser.parseGrammar str
let printGrammarFile name =
    let (g,pOrder) = parseGrammar name
    for p in g do
        printfn "%A" p
    printfn "----"

printGrammarFile "grammar1"
printGrammarFile "grammar2"
printGrammarFile "grammar3"
printGrammarFile "grammar4"
printGrammarFile "grammarFail2"
