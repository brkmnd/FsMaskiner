open Maskiner
open System.Collections.Generic
open Grammar2Slr
open GrammarParserExt
(* File IO
 * *)
let grammar2str fName =
    let l = System.IO.File.ReadAllLines (fName)
    Array.fold
        (fun acc line -> acc + line + "\n")
        ""
        l
let str2js str =
    let name = "tableout.js"
    System.IO.File.WriteAllText(name,str)

(* Indents
 * *)
let ind1 = "    "
let ind2 = ind1 + ind1
let ind3 = ind1 + ind2
let ind4 = ind2 + ind2
let ind5 = ind2 + ind3
let ind6 = ind3 + ind3

(* ParserFunction
 * *)
let parseGrammar grammarStr =
    let (plist,pOrder) =
        try parse grammarStr with
        | Failure msg ->
            printfn "%s" msg
            ([EmptyTree],[])
    match plist with
    | [ DataProds prods
        DataTokens tokens
        DataBTokens btokens
        DataPrecs precs
        DataAssocs assocs
        DataGroups groups] ->
            (prods,pOrder,tokens,btokens,precs,assocs,groups)
    | t ->
        printfn "t : %A" t 
        failwith "some error"
(* Input atts from parser
 * *)
let (parserName,inputFileName) = ("GrammarParserExt","grammars/mbol")
//let (parserName,inputFileName) = ("GrammarParserExt","grammars/grammarparser_ext")
//let (parserName,inputFileName) = ("GrammarParserExt","grammars/comcalc")
let fileInput = grammar2str inputFileName
(* Parse input
 * *)
let (prods,pOrder,tokenDefs,bTokenDefs,precs,assocs,groups) = parseGrammar fileInput

(* Format functions
 **)
let newErrorMsg expected given =
    if expected <> "" && given <> "" then
        //sprintf "Error \"unexpected %s in input\"" token
        sprintf "error(\"expected %s but given '%s'\")" expected given
    else "error(\"\")"
let formatAction (expSet : HashSet<string>) =
    let expSet2str =
        Set<string>.fold
            (fun (acc : string) x ->
                let s =
                    if x = "$" then "eoi"
                    elif groups.ContainsKey(x) then
                        groups.[x]
                    else x
                if acc.Contains(s) then acc
                else acc + "'" + s + "',"
                )
            ""
            expSet
    (fun symbol (s : string) ->
        if s.[0] = 'E' then
            let symbol0 =
                if groups.ContainsKey(symbol) then
                    groups.[symbol]
                else symbol
            newErrorMsg expSet2str symbol0
        elif s.[0] = 's' then
            sprintf "shift(%s)" s.[1 ..]
        elif s = "r0" then "accept()"
        else sprintf "reduce(%s)" s.[1..]
        )
let formatGoto = function
    | "Ø" -> "none()"
    | s -> sprintf "some(%s)" s.[1..]
(* Conflict solvers
 * *)
let conflictF logic (productionOps : (string list) []) formatF symbol (shiftKey : string) (reduceKey : string) =
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
        if precs.ContainsKey(curOp) then
            precs.[curOp]
        else
            failwith (sprintf "precedence rule for %s not found" curOp)
    let lastPrec =
        if precs.ContainsKey(lastOp) then
            precs.[lastOp]
        else
            failwith (sprintf "precedence rule for %s not found" lastOp)
    let curAssoc =
        if assocs.ContainsKey(curOp) then
            assocs.[curOp]
        else
            failwith (sprintf "associativity rule for %s not found" curOp)
    logic formatF curPrec lastPrec curAssoc shiftIdStr reduceIdStr
let conflLogic formatF curPrec lastPrec curAssoc shiftIdStr reduceIdStr =
    let reduceA () = formatF ("r" + reduceIdStr)
    let shiftA () = formatF ("s" + shiftIdStr)
    if curPrec = lastPrec then
        if curAssoc = "left" then
            reduceA ()
        else
            shiftA ()
    elif curPrec > lastPrec then
        shiftA ()
    else
        reduceA ()
(* Create the tables and used lists
 * *)
let (mergedTables,gotoTable,unfoldedGrammar) =
    createTables
        (prods,pOrder)
        (conflictF conflLogic)
        formatAction
        formatGoto
(* Components
 * Components
 * Components
 * Components
 * *)        
(*JS Components
 * *)
let js_prods_fun =
    let mutable retval = "var productions_fun = {\n"
    let mutable i0 = 0
    for prod in unfoldedGrammar do
        let rSide =
            List.fold
                (fun acc x ->
                    match x with
                    | GrammarParser.Term t -> sprintf "%s%s" acc t
                    | GrammarParser.NonTerm t -> sprintf "%s%s " acc t
                    | _ -> acc
                    )
                (sprintf "%s//[%d] %s -> " ind1 i0 (fst prod))
                (snd prod)
        retval <-
            sprintf
                "%s%s\n%s%d:function(tree){ return tree; },\n"
                retval
                rSide
                ind1
                i0
        i0 <- i0 + 1
    retval.[0 .. retval.Length - 3] + "\n};\n"
let js_prods_list =
    let mutable retval = "var productions_str = {\n"
    let mutable i0 = 0
    for prod in unfoldedGrammar do
        let rSide =
            List.fold
                (fun acc x ->
                    match x with
                    | GrammarParser.Term t ->
                        sprintf "%s\"%s\"," acc t
                    | GrammarParser.NonTerm t ->
                        sprintf "%s\"%s\"," acc t
                    | _ -> acc
                    )
                ""
                (snd prod)
        retval <-
            sprintf
                "%s%s%d:{prod:\"%s\",rside:[%s]},\n"
                retval
                ind1
                i0
                (fst prod)
                rSide.[0 .. rSide.Length - 2]
        i0 <- i0 + 1
    retval.[0 .. retval.Length - 3] + "\n};\n"
let js_actionTable =
    let mutable i0 = 0
    let mutable retval = "var actionTable = {\n"
    for row in mergedTables do
        retval <- sprintf "%s%s%d:{\n" retval ind1 i0
        for symbol in row do
            retval <-
                sprintf
                    "%s%s\"%s\":function(lang){ return lang.%s; },\n"
                    //"%s%s\"%s\":t.%s,\n"
                    retval
                    ind2
                    symbol.Key
                    symbol.Value
        retval <-
            sprintf
                "%s\n%s},\n"
                    retval.[0 .. retval.Length - 3]
                    ind2
        i0 <- i0 + 1
    sprintf
        "%s\n%s};\n"
        retval.[0 .. retval.Length - 3]
        ind1
let js_gotoTable =
    let mutable i0 = 0
    let mutable retval = "var gotoTable = {\n"
    for row in gotoTable do
        retval <- sprintf "%s%s%d:{\n" retval ind1 i0
        for symbol in row do
            retval <-
                sprintf
                    "%s%s\"%s\":function(lang){ return lang.%s; },\n"
                    retval
                    ind2
                    symbol.Key
                    symbol.Value
        retval <-
            sprintf
                "%s\n%s},\n"
                retval.[0 .. retval.Length - 3]
                ind2
        i0 <- i0 + 1
    sprintf
        "%s\n%s};\n"
        retval.[0 .. retval.Length - 3]
        ind1
let js_tokens =
    let mutable retval_tokens = "var tokens = [\n"
    for tokenG in mergedTables.[0] do
        let (tokenK,tokenV) =
            let k = tokenG.Key
            if tokenDefs.ContainsKey(k) then
                (k,tokenDefs.[k])
            else (k,(false,k))
        if tokenK <> "$" then
            retval_tokens <-
                sprintf
                    "%s{name:\"%s\",cap:%s,reg:\"%s\"},\n"
                    retval_tokens
                    tokenK
                    ((fun t ->
                        if t then "true"
                        else "false"
                        )(fst tokenV))
                    (snd tokenV)
    retval_tokens.[0 .. retval_tokens.Length - 3] + "\n]\n"
let js_btokens =
    let mutable retval = "var btokens = ["
    for btoken in bTokenDefs do
        let repStr = btoken.Replace("\\","\\\\")
        retval <- sprintf "%s\"%s\"," retval repStr
    retval.[0 .. retval.Length - 2] + "];"
        
(* Append components
 * *)
let c_js_table =
    js_prods_list +
    js_actionTable +
    js_gotoTable +
    js_tokens +
    js_btokens
        
(* Write to output file
 * *)        
//str2html c_final
str2js c_js_table
