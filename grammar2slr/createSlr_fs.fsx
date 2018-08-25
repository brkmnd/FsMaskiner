open Maskiner
open System.Collections.Generic
open Grammar2Slr
open GrammarParserExt


(* FileFunctions
 * *)
let fopen fName =
    let l = System.IO.File.ReadAllLines (fName)
    Array.fold
        (fun acc line -> acc + line + "\n")
        ""
        l
let fwrite str =
    let name = "slrparser.fs"
    System.IO.File.WriteAllText(name,str)
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
(* IO - Attributes
 * *)
let (parserName,inputFileName) = ("ComCalcParser","grammars/comcalc")
//let (parserName,inputFileName) = ("GrammarParserExt","grammars/grammarparser_ext")
//let (parserName,inputFileName) = ("GrammarParserExt","grammars/s_error1")
let fileInput = fopen inputFileName
(* Parse input
 * *)
let (prods,pOrder,tokenDefs,bTokenDefs,precs,assocs,groups) = parseGrammar fileInput
(* Print to output string functions
 * All work on mutable values
 * *)

let action2str_table (table : Dictionary<string,string>[]) =
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
let goto2str_table (table : Dictionary<string,string>[]) =
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
let prods2str_list prods =
    let mutable retval = "    let productions_str = [|\n"
    let mutable i0 = 0
    for prod in prods do
        let rSide =
            List.fold
                (fun acc x ->
                    match x with
                    | GrammarParser.Term t | GrammarParser.NonTerm t -> sprintf "%s\"%s\";" acc t
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
let prods2str_fun prods =
    let mutable retval = "    let productions_fun = [|\n"
    let mutable i0 = 0
    for prod in prods do
        let rSide =
            List.fold
                (fun acc x ->
                    match x with
                    | GrammarParser.Term t | GrammarParser.NonTerm t -> sprintf "%s%s " acc t
                    | _ -> acc
                    )
                (sprintf "        //[%d] %s -> " i0 (fst prod))
                (snd prod)
        retval <- sprintf "%s%s\n        (fun tree -> [EmptyTree])\n" retval rSide
        i0 <- i0 + 1
    retval + "        |]"
(*
let tokens2str_type_table (actionTable : Dictionary<string,string>[]) =
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
*)
let token2str_regex tokens (tokenDefs : Dictionary<string,bool * string>) (bTokenDefs : List<string>) =
    // in the case where (as) | (assoc) the latter will never be matched, and we have garbage
    let mutable i0 = 1
    let ind1 = "    "
    let ind2 = ind1 + ind1
    let ind3 = ind1 + ind2
    let ind4 = ind2 + ind2
    let mutable retvalRegStr =
        ind2 +
        "let regToken =\n"
    let mutable retvalRegAdd =
        ind2 +
        "let addToken (tGroup : GroupCollection) =\n"
    for tName in tokens do
        if tokenDefs.ContainsKey(tName) && tokenDefs.[tName]|>(fun (_,n) -> n <> "") then
            retvalRegStr <-
                retvalRegStr +
                (sprintf "%s\"(%s)|\"+\n" ind4 (snd tokenDefs.[tName]))
        else
            retvalRegStr <-
                retvalRegStr +
                (sprintf "%s\"(%s)|\"+\n" ind4 tName)
        retvalRegAdd <-
            retvalRegAdd +
            (sprintf
                "%sif tGroup.[%d].Value <> \"\" then\n"
                ind3
                i0
                )
        if tokenDefs.ContainsKey(tName) && fst tokenDefs.[tName] then
            retvalRegAdd <-
                retvalRegAdd +
                (sprintf
                    "%stokensL.Add((\"%s\",Some tGroup.[%d].Value))\n"
                    ind4
                    tName
                    i0
                    )
        else
            retvalRegAdd <-
                retvalRegAdd +
                (sprintf
                    "%stokensL.Add((\"%s\",None))\n"
                    ind4
                    tName
                    )
        i0 <- i0 + 1
    for bToken in bTokenDefs do
        retvalRegStr <-
            retvalRegStr +
            (sprintf "%s\"%s|\"+\n" ind4 bToken)
    retvalRegStr <-
        retvalRegStr +
        (sprintf "%s\"$\"\n" ind4)
    (retvalRegAdd,retvalRegStr)

(* Conflict solver and format of table entries
 * *)
let newErrorMsg expected given =
    if expected <> "" && given <> "" then
        //sprintf "Error \"unexpected %s in input\"" token
        sprintf "Error \"expected %s but given '%s'\"" expected given
    else "Error \"\""
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
        elif s.[0] = 's' then "Shift " + s.[1..]
        elif s = "r0" then "Accept"
        else "Reduce " + s.[1..]
        )
let formatGoto = function
    | "Ø" -> "None"
    | s -> "Some " + s.[1..]
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
            printfn "reduceId : %A"  reduceId
            let mutable i0 = 0
            for pop in productionOps do
                printfn "pop(%d): %A" i0 pop
                i0 <- i0 + 1
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
(* Create the tables and used lists
 * *)
let (mergedTables,gotoTable,unfoldedGrammar) =
    createTables
        (prods,pOrder)
        (conflictF conflLogic)
        formatAction
        formatGoto
let tokens =
    Dict<string,string>.fold
        (fun acc x ->
            if x.Key = "$" then acc
            else acc @ [x.Key]
            )
        []
        mergedTables.[0]
(* Components for the final output - strings
 * *)
let c_parserName = parserName
let c_parserModule =
    "module " + parserName + " =\n"+
    "    open System.Collections.Generic\n"+
    "    open System.Text.RegularExpressions\n"
let c_actionType =
    "    type Action =\n"+
    "        | Shift of int\n"+
    "        | Reduce of int\n"+
    "        | Accept\n"+
    "        | Error of string\n"
let c_treeType =
    "    type Tree =\n"+
    "        | EmptyTree\n"
let c_explAddLeaf = fopen("code_fs/explAddLeaf")
let c_explFun = fopen("code_fs/explFun")
let c_addLeafFun = fopen("code_fs/addLeaf")
let c_parseAlgHead = fopen("code_fs/parsingalg_header")
let c_parseAlgBottom = fopen ("code_fs/parsingalg_footer")
let c_initTreeStack = fopen ("code_fs/initTreeStack")
//let (c_tokenu,c_tokenConvAlg) = tokens2str_type_table mergedTables
let c_lexerHeader = fopen ("code_fs/lexer_header")
let c_lexerFooter = fopen ("code_fs/lexer_footer")
let (c_lexerRegAdd,c_lexerRegStr) =
    token2str_regex 
        tokens
        tokenDefs
        bTokenDefs
let c_actionTable_str = action2str_table mergedTables
let c_gotoTable_str = goto2str_table gotoTable
let c_parseAlg =
    c_parseAlgHead +
    //c_tokenConvAlg + replace with proper
    c_parseAlgBottom

let components2str =
    c_parserModule +
    //c_tokenType +
    c_actionType +
    c_treeType +
    c_initTreeStack +
    c_explAddLeaf +
    c_addLeafFun +
    c_explFun + 
    (prods2str_fun unfoldedGrammar) + "\n" +
    c_lexerHeader +
    c_lexerRegAdd +
    c_lexerRegStr + 
    c_lexerFooter +
    c_actionTable_str + "\n" +
    c_gotoTable_str + "\n" + 
    (prods2str_list unfoldedGrammar) + "\n" +
    c_parseAlg

fwrite components2str
printfn "done"
//printfn "token_regex = \n%s" regAdd
//printfn "token_regex = \n%s" regStr
