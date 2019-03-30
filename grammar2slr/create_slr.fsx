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
        DataTokensOrder tOrder
        DataBTokens btokens
        DataPrecs precs
        DataAssocs assocs
        DataGroups groups] ->
            (prods,pOrder,tokens,tOrder,btokens,precs,assocs,groups)
    | t ->
        printfn "t : %A" t 
        failwith "some error"
let exe_app parserName inputFileName =
    let fileInput = grammar2str inputFileName
    (* Create the tables and used lists
     * *)
    //let (mergedTables,gotoTable,unfoldedGrammar) =
    let (mergedTables,gotoTable,unfoldedGrammar,tokenDefs,tokenDefsOrder,bTokenDefs) =
        Grammar2JsTable.createTables
            fileInput
    (* Components
     * Components
     * Components
     * Components
     * *)        
    (*JS Components
     * *)
    let js_prods_str =
        let mutable retval = "var productions_str = {\n"
        let mutable i0 = 0
        for prod in unfoldedGrammar do
            let rSide =
                let mutable retval = ""
                for t in (snd prod) do
                    retval <- retval + "\"" + t + "\","
                retval
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
        let bool2str b =
            if b then "true"
            else "false"
        let tokensFromTable =
            if mergedTables.Length > 0 then mergedTables.[0]
            else failwith "eemerged tables are empty"
        for token_key in tokenDefsOrder do
            let token_value = tokenDefs.[token_key]
            let token_cap = fst token_value
            let token_reg = snd token_value
            //exclude tokens that are defined but not present in table
            if token_key <> "$" && tokensFromTable.ContainsKey(token_key) then
                retval_tokens <-
                    sprintf
                        "%s{name:\"%s\",cap:%s,reg:\"%s\"},\n"
                        retval_tokens
                        token_key
                        (bool2str token_cap)
                        token_reg
        //fix it so tokens not defined are included
        for token in tokensFromTable do
            let token_key = token.Key
            if token_key <> "$" && not (tokenDefs.ContainsKey(token_key)) then
                let token_cap = false
                let token_reg = token_key
                retval_tokens <-
                    sprintf
                        "%s{name:\"%s\",cap:%s,reg:\"%s\"},\n"
                        retval_tokens
                        token_key
                        (bool2str token_cap)
                        token_reg
        retval_tokens.[0 .. retval_tokens.Length - 3] + "\n]\n"
    let js_btokens =
        let mutable retval = "var btokens = ["
        if bTokenDefs.Count = 0 then
            retval + "]"
        else
            for btoken in bTokenDefs do
                let repStr = btoken.Replace("\\","\\\\")
                retval <- sprintf "%s\"%s\"," retval repStr
            retval.[0 .. retval.Length - 2] + "];"
            
    (* Append components
     * *)
    let c_js_table =
        js_prods_str +
        js_actionTable +
        js_gotoTable +
        js_tokens +
        js_btokens
            
    (* Write to output file
     * *)        
    str2js c_js_table

[<EntryPoint>]
let main argv =
    match argv with
    | [|fileName|] ->
        exe_app "Parser0" fileName
        printfn "done"
    | _ ->
        printfn "This app takes exactly one argument: the path of the input grammar"
    0
