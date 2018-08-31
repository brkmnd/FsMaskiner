namespace Maskiner
module Grammar2JsTable =
    open System.Collections.Generic
    open Grammar2Slr
    open GrammarParserExt
    (* Parsing
     * Parsing
     * *)
    let parseGrammar grammarStr =
        let ((plist,pOrder),msg) =
            try (parse grammarStr,"") with
            | Failure msg ->
                (([EmptyTree],[]),msg)
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
            failwith (sprintf "parser error: %s" msg)
    (* Format functions
     * Format functions
     **)
    let newErrorMsg expected given =
        if expected <> "" && given <> "" then
            //sprintf "Error \"unexpected %s in input\"" token
            sprintf "error(\"expected %s but given '%s'\")" expected given
        else "error(\"\")"
    let formatAction (groups : Dictionary<string,string>) (expSet : HashSet<string>) =
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
    let conflictF (assocs : Dictionary<string,string>) (precs : Dictionary<string,int>) logic (productionOps : (string list) []) formatF symbol (shiftKey : string) (reduceKey : string) =
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
    let createTables inStr =
        let (prods,pOrder,tokenDefs,tokenDefsOrder,bTokenDefs,precs,assocs,groups) = parseGrammar inStr
        let (mergedTables,gotoTable,unfoldedGrammar) =
            createTables
                (prods,pOrder)
                (conflictF assocs precs conflLogic)
                (formatAction groups)
                formatGoto
        let conv_unfoldedGrammar =
            List.fold
                (fun (acc : List<string * List<string>>) (lside,rside) ->
                    let new_rside =
                        List.fold
                            (fun (acc : List<string>) x ->
                                match x with
                                | GrammarParser.Term t -> acc.Add(t); acc
                                | GrammarParser.NonTerm t -> acc.Add(t); acc
                                | _ -> acc
                                )
                            (new List<string>())
                            rside
                    acc.Add((lside,new_rside))
                    acc
                    )
                (new List<string * List<string>>())
                unfoldedGrammar
        (mergedTables,gotoTable,conv_unfoldedGrammar,tokenDefs,tokenDefsOrder,bTokenDefs)
