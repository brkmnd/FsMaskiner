open Maskiner
open GrammarParser
open Grammar2Slr
open System.Collections.Generic
let fopen name =
    let l = System.IO.File.ReadAllLines (name)
    Array.fold
        (fun acc line -> acc + line + "\n")
        ""
        l
let fwrite str =
    let name = "fsparser.out"
    System.IO.File.WriteAllText(name,str)
        
let action2fsharpStr (table : Dictionary<string,string>[]) =
    let mutable i0 = 0
    let mutable retval = "    let actionTable = [|\n"
    for row in table do
        retval <- retval + (sprintf "        //s%s\n" ((string) i0))
        retval <- retval + "        (fun (dict : Dictionary<string,Action>) ->\n"
        for symbol in row do
            retval <- retval + (sprintf "            dict.Add(\"%s\",%s)\n" symbol.Key symbol.Value)
        retval <- retval + "            dict\n"
        retval <- retval + "            )(new Dictionary<string,Action>())\n"
        i0 <- i0 + 1
    retval + "        |]"
let goto2fsharpStr (table : Dictionary<string,string>[]) =
    let mutable i0 = 0
    let mutable retval = "    let gotoTable = [|\n"
    for row in table do
        retval <- retval + (sprintf "        //s%s\n" ((string) i0))
        retval <- retval + "        (fun (dict : Dictionary<string,Option<int>>) ->\n"
        for symbol in row do
            retval <- retval + (sprintf "            dict.Add(\"%s\",%s)\n" symbol.Key symbol.Value)
        retval <- retval + "            dict\n"
        retval <- retval + "            )(new Dictionary<string,Option<int>>())\n"
        i0 <- i0 + 1
    retval + "        |]"
let prodStr2fsharpStr prods =
    let mutable retval = "    let productions_str = [|\n"
    let mutable i0 = 0
    for prod in prods do
        let rSide =
            List.fold
                (fun acc x ->
                    match x with
                    | Term t | NonTerm t -> sprintf "%s\"%s\";" acc t
                    | _ -> acc
                    )
                (sprintf "        (\"%s\",[|" (fst prod))
                (snd prod)
        let rSide =
            if rSide.[rSide.Length - 1] <> '|' then
                rSide.[0 .. rSide.Length - 2]
            else rSide
        retval <- sprintf "%s%s|]) //%d\n" retval rSide i0
        i0 <- i0 + 1
    retval + "        |]"
let prodFun2fsharpStr prods =
    let mutable retval = "    let productions_fun = [|\n"
    let mutable i0 = 0
    for prod in prods do
        let rSide =
            List.fold
                (fun acc x ->
                    match x with
                    | Term t | NonTerm t -> sprintf "%s%s " acc t
                    | _ -> acc
                    )
                (sprintf "        //[%d] %s -> " i0 (fst prod))
                (snd prod)
        retval <- sprintf "%s%s\n        (fun tree -> [EmptyTree])\n" retval rSide
        i0 <- i0 + 1
    retval + "        |]"
let tokens2fsharpStr (actionTable : Dictionary<string,string>[]) =
    let mutable retvalType = "    type Tokens =\n"
    let mutable retvalLookup = "        let token2lookup = function\n"
    for token in actionTable.[0] do
        let t0 = token.Key
        let t0_form =
            if t0 = "$" then "dollar"
            else t0
        retvalType <- sprintf "%s        | Token_%s\n" retvalType t0_form
        retvalLookup <- sprintf "%s            | Token_%s -> \"%s\"\n" retvalLookup t0_form t0
    (retvalType,retvalLookup)

let precedence = new Dictionary<string,int>()
precedence.Add("plus",1)
precedence.Add("minus",1)
precedence.Add("times",2)
precedence.Add("divide",2)
precedence.Add("power",3)
let associativity = new Dictionary<string,string>()
associativity.Add("plus","left")
associativity.Add("minus","left")
associativity.Add("times","left")
associativity.Add("divide","left")
associativity.Add("power","right")

let formatAction (s : string) =
    if s. [0] = 'E' then "Error \"\""
    elif s.[0] = 's' then "Shift " + s.[1..]
    elif s = "r0" then "Accept"
    else "Reduce " + s.[1..]
let formatActionOut (s : string) =
    if s = "r0" then "acc"
    else s
let formatGoto = function
    | "Ø" -> "None"
    | s -> "Some " + s.[1..]
let conflLogic1 formatF curPrec lastPrec curAssoc shiftIdStr reduceIdStr =
    let reduceA = "r" + reduceIdStr
    let shiftA = "s" + shiftIdStr
    if curPrec = lastPrec then
        if curAssoc = "left" then
            reduceA
        else
            shiftA
    elif curPrec > lastPrec then
        shiftA
    else
        reduceA
let conflLogic2 formatF curPrec lastPrec curAssoc shiftIdStr reduceIdStr =
    let reduceA = formatF ("r" + reduceIdStr)
    let shiftA = formatF ("s" + shiftIdStr)
    shiftA + "/" + reduceA

let conflF logic (productionOps : (string list) []) formatF symbol (shiftKey : string) (reduceKey : string) =
    let (shiftId,shiftIdStr) =
        let idStr = shiftKey.[1..]
        ((int) idStr,idStr)
    let (reduceId,reduceIdStr) =
        let idStr = reduceKey.[1..]
        ((int) idStr,idStr)
    let curOp = symbol
    //might change depending on how many ops allowed per prod
    let lastOp =
        let rSide = productionOps.[reduceId]
        if rSide.Length = 0 then
            failwith "internal error: reduce on empty op list"
        else rSide.[0]
    let curPrec =
        if precedence.ContainsKey(curOp) then
            precedence.[curOp]
        else
            failwith (sprintf "precedence rule for %s not found" curOp)
    let lastPrec =
        if precedence.ContainsKey(lastOp) then
            precedence.[lastOp]
        else
            failwith (sprintf "precedence rule for %s not found" lastOp)
    let curAssoc =
        if associativity.ContainsKey(curOp) then
            associativity.[curOp]
        else
            failwith (sprintf "associativity rule for %s not found" curOp)
    logic formatF curPrec lastPrec curAssoc shiftIdStr reduceIdStr

//let grammar1 = fopen "tests/test1"
//let parsed1 = parseGrammar grammar1
//let (mergedTables,gotoTable) = createTables parsed1 (conflF conflLogic1) formatF1

let grammar_self = fopen "tests/grammar_self"
let parsed_self = parseGrammar grammar_self
let (mergedTables,gotoTable,unfoldedGrammar) = createTables parsed_self (conflF conflLogic1) formatAction formatGoto




let parserName = "GrammarParserExt"
let parserModule =
    "module " + parserName + " =\n"+
    "    open System.Collections.Generic\n"+
    "    open System.Text.RegularExpressions\n"
let actionType =
    "    type Action =\n"+
    "        | Shift of int\n"+
    "        | Reduce of int\n"+
    "        | Accept\n"+
    "        | Error of string\n"
let treeType =
    "    type Tree =\n"+
    "        | EmptyTree\n"
let explAddLeaf = fopen("code/explAddLeaf")
let explFun = fopen("code/explFun")
let addLeafFun = fopen("code/addLeaf")
let parseAlgHead = fopen("code/parsingalg_head")
let parseAlgBottom = fopen ("code/parsingalg_bottom")
let initTreeStack = fopen ("code/initTreeStack")
let (tokenType,tokenConvAlg) = tokens2fsharpStr mergedTables
let actionTable_str = action2fsharpStr mergedTables
let gotoTable_str = goto2fsharpStr gotoTable
let parseAlg =
    parseAlgHead +
    tokenConvAlg +
    parseAlgBottom

let writeStr =
    parserModule +
    tokenType +
    actionType +
    treeType +
    initTreeStack +
    explAddLeaf +
    addLeafFun +
    explFun + 
    (prodFun2fsharpStr unfoldedGrammar) + "\n" +
    actionTable_str + "\n" +
    gotoTable_str + "\n" + 
    (prodStr2fsharpStr unfoldedGrammar) + "\n" +
    parseAlg

fwrite writeStr
printfn "done"


let printTable (table : Dictionary<string,string>[]) =
    let mutable i0 = 0
    printfn "table"
    for field in table.[0] do
        let k =
            let r = field.Key
            if r.Length > 4 then r.[0 .. 3]
            else r
        printf "%-5s" k
    printfn ""
    for row in table do
        for f in row do
            printf "%-5s" f.Value
        printfn ""
    printfn "end"

//printTable mergedTables
