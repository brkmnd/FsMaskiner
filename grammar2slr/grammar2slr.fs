namespace Maskiner
module Grammar2Slr =
    open System.Collections.Generic
    open Nfa2Dfa
    open GrammarParser

    (* Helper functions
     * *)
    let symbol2str = function
        | NonTerm s -> "[nt]" + s
        | Term s -> "[te]" + s
        | _ -> "[$$]"
    let isNT (trans : string) =
        (trans.[0..3] = "[nt]",trans.[4..])
    let isTE (trans : string) =
        (trans.[0..3] = "[te]",trans.[4..])
    (* input  : parsed grammar
     * output : nfaTree 
     * *)
    let grammar2nfa unfoldedGrammar =
        let (nfaTree,prodStates,accStates,_,_) =
            List.fold
                (fun (tree : Dictionary<string,State>,pStates,aStates,nameI,prodI) (prod,rightSide) ->
                    let nameI0 =
                        List.fold
                            (fun nameI tSymbol ->
                                let stateName = sprintf "%x" nameI
                                let targetName = sprintf "%x" (nameI + 1)
                                let acc = false
                                let start =
                                    if nameI = 1 then true
                                    else false
                                let newS =
                                    newState
                                        acc
                                        start
                                        [Char (tSymbol|>symbol2str,targetName)]
                                tree.Add(stateName,newS)
                                nameI + 1
                                )
                            nameI
                            rightSide
                    let accStateName = sprintf "%x" nameI0
                    let pStates0 = (prod,sprintf "%x" (nameI))::pStates
                    let aStates0 = (accStateName,prodI)::aStates
                    let prodI0 = prodI + 1
                    let nameI0 = nameI0 + 1
                    tree.Add(accStateName,newState true false [])
                    (tree,pStates0,aStates0,nameI0,prodI0)
                    )
                (new Dictionary<string,State>(),[],[],1,0)
                unfoldedGrammar
        (nfaTree,prodStates,accStates)
    (*  input  : nfaTree
     *  output : nfaTree with epsilon transitions
     * *)
    let nfa2nfa_eps (nfaTree,prodStates) =
        Dict<string,State>.fold
            (fun (nTree : Dictionary<string,State>) node ->
                let curState = node.Value
                let curAcc = curState.accepting
                let curStart = curState.start
                let curTrans = curState.transitions
                match curTrans with
                | [Char (trans,target)] ->
                    let (nterm,rest) = isNT trans
                    if nterm then
                        let addEpsTrans =
                            List.fold
                                (fun acc (nt,target) ->
                                    if nt = rest then
                                        (Epsilon target)::acc
                                    else
                                        acc
                                    )
                                []
                                prodStates
                        let newTrans = curTrans @ addEpsTrans
                        nTree.Add(node.Key,newState curAcc curStart newTrans)
                        nTree
                    else
                        nTree.Add(node.Key,node.Value)
                        nTree
                | _ ->
                    nTree.Add(node.Key,node.Value)
                    nTree
                )
            (new Dictionary<string,State>())
            nfaTree
    (* input  : nfaTree with epsilon transitions
     * output : dfa
     * *)
    let nfa2dfa (nfaTreeWithEps) =
        let echoes = new Dictionary<string,System.Func<string[],bool>>()
        Nfa2Dfa.convert "dfa0" nfaTreeWithEps echoes
    (* input  : dfa
     * output : actionTable - dict<string,string> []
     * *)
    let dfa2actionTable (dfaTree : (string * SortedDictionary<string,State> * Dictionary<string,string>) []) =
        let translateField = function
            | "Ø" -> "E"
            | s -> s
        Array.init
            dfaTree.Length
            (fun i ->
                let (stateName,_,transitions) = dfaTree.[i]
                Dict<string,string>.fold
                    (fun (row : Dictionary<string,string>) trans ->
                        let (isTerm,rest) = isTE trans.Key
                        if isTerm then
                            let field = translateField trans.Value
                            row.Add(rest,field)
                            row
                        else
                            row
                        )
                    (new Dictionary<string,string>())
                    transitions
                )
    (* input  : dfaTree
     * output : gotoTable - dict<srting,string> []
     * *)
    let dfa2gotoTable (dfaTree : (string * SortedDictionary<string,State> * Dictionary<string,string>) []) formatF =
        Array.init
            dfaTree.Length
            (fun i ->
                let (stateName,_,transitions) = dfaTree.[i]
                Dict<string,string>.fold
                    (fun (row : Dictionary<string,string>) trans ->
                        let (isNonTerm,rest) = isNT trans.Key
                        if isNonTerm then
                            let field = formatF trans.Value
                            row.Add(rest,field)
                            row
                        else
                            row
                        )
                    (new Dictionary<string,string>())
                    transitions
                )
    let dfa2reduceTable (dfaTree : (string * SortedDictionary<string,State> * Dictionary<string,string>) []) (grammar,unfoldedGrammar : (string * ProdExp list) list,nfaAccStates) =
        let eMsg = "reduce error:"
        let nullable = Grammar2Set.calcNullable grammar
        let first = Grammar2Set.calcFirst grammar nullable
        let follow = Grammar2Set.calcFollow grammar first nullable
        Array.init
            dfaTree.Length
            (fun i ->
                let (stateName,eClosure,transitions) = dfaTree.[i]
                List.fold
                    (fun (row : Dictionary<string,string>) (accState,p_i) ->
                        if eClosure.ContainsKey(accState) then
                            let prod = fst unfoldedGrammar.[p_i]
                            let prodFollow =
                                if not (follow.ContainsKey(prod)) then
                                    //for g in grammar do
                                    //    printfn "%s -> %A" g.Key g.Value
                                    failwith (sprintf "%s missing follow set of '%s'" eMsg prod)
                                    //new HashSet<ProdExp>()
                                else follow.[prod]
                            Set<ProdExp>.fold
                                (fun (row : Dictionary<string,string>) x ->
                                    match x with
                                    | Term t ->
                                        if row.ContainsKey(t) then
                                            failwith (sprintf "%s possibly overlapping la-set on '%s'" eMsg t)
                                        else
                                            row.Add(t,sprintf "r%d" p_i); row
                                    | Dollar -> row.Add("$",sprintf "r%d" p_i); row
                                    | _ -> row
                                    )
                                row
                                prodFollow
                        else row
                        )
                    (new Dictionary<string,string>())
                    nfaAccStates
                )
    let mergeTables (shiftT : Dictionary<string,string>[]) (reduceT : Dictionary<string,string>[]) conflF formatF =
        Array.init
            shiftT.Length
            (fun i ->
                let shiftRow = shiftT.[i]
                let reduceRow = reduceT.[i]
                shiftRow.Add("$","E")
                let expInRow =
                    Dict<string,string>.fold
                        (fun (acc : HashSet<string>) elm ->
                            if elm.Value <> "E" || reduceRow.ContainsKey(elm.Key) then
                                let add = acc.Add(elm.Key)
                                acc
                            else acc
                            )
                        (new HashSet<string>())
                        shiftRow

                let formatFA0 = formatF expInRow
                Dict<string,string>.fold
                    (fun (newRow : Dictionary<string,string>) elm ->
                        let symbol = elm.Key
                        let shiftRowV = elm.Value
                        let formatFA = formatFA0 symbol
                        if reduceRow.ContainsKey(symbol) && shiftRowV <> "E" then
                            newRow.Add(symbol,conflF (formatFA) elm.Key shiftRowV reduceRow.[symbol])
                            newRow
                        elif shiftRowV = "E" && reduceRow.ContainsKey(symbol) then
                            newRow.Add(symbol,formatFA reduceRow.[symbol])
                            newRow
                        else
                            newRow.Add(symbol,formatFA shiftRowV)
                            newRow
                        )
                    (new Dictionary<string,string>())
                    shiftRow
                )
    let createTables (grammar : Dictionary<string,(ProdExp list) list>,prodOrder) conflF formatAction formatGoto =
        let prodOrder =
            match prodOrder with
            | [] -> ["__"]
            | head::pOrder -> "__"::head::pOrder
        let newGrammar =
            Dict<string,(ProdExp list) list>.fold
                (fun (d : Dictionary<string,(ProdExp list) list>) x ->
                    d.Add(x.Key,x.Value)
                    d
                    )
                ((fun () ->
                    let d = new Dictionary<string,(ProdExp list) list>()
                    let rSide =
                        if List.length prodOrder = 1 then [[]]
                        else [[NonTerm prodOrder.[1]]]
                    d.Add("__",rSide)
                    d
                    )() 
                )
                grammar
        let unfoldedGrammar =
            let retval = 
                List.fold
                    (fun acc prod ->
                        List.fold (fun acc prodE -> (prod,prodE)::acc) acc newGrammar.[prod]
                        )
                    []
                    prodOrder
            List.rev retval
        let (productionOps,_) =
            List.fold
                (fun (arr : (string list) [],i) prod ->
                    let prodOps =
                        List.fold
                            (fun ops e ->
                                match e with
                                | Term t -> t::ops
                                | _ -> ops
                                )
                            []
                            (snd prod)
                    arr.[i] <- prodOps
                    (arr,i+1)
                    )
                (Array.init unfoldedGrammar.Length (fun i -> [""]),0)
                unfoldedGrammar
        let (nfa,nfaProdStates,nfaAccStates) = grammar2nfa unfoldedGrammar
        let nfa_eps = nfa2nfa_eps (nfa,nfaProdStates)
        let dfa = nfa2dfa nfa_eps
        let shiftTable = dfa2actionTable dfa
        let gotoTable = dfa2gotoTable dfa formatGoto
        let reduceTable = dfa2reduceTable dfa (newGrammar,unfoldedGrammar,nfaAccStates)
        let mergedTables = mergeTables shiftTable reduceTable (conflF productionOps) formatAction 
        (mergedTables,gotoTable,unfoldedGrammar) 
