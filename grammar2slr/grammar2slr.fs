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
    let grammar2nfa1 (grammarDict : Dictionary<string,(ProdExp list) list>,prodOrder : string list) =
        let (nfaTree,prodStates,accStates,_,_) =
            List.fold
                (fun (tree : Dictionary<string,State>,pStates,aStates,nameI,prodI) prod ->
                    let (pStates0,aStates0,nameI0,prodI0) =
                        List.fold
                            (fun (pStates,aStates,nameI,prodI) rSide ->
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
                                        rSide
                                let accStateName = sprintf "%x" nameI0
                                let pStates0 = (prod,sprintf "%x" (nameI))::pStates
                                let aStates0 = (accStateName,prodI)::aStates
                                let prodI0 = prodI + 1
                                let nameI0 = nameI0 + 1
                                tree.Add(accStateName,newState true false [])
                                (pStates0,aStates0,nameI0,prodI0)
                                )
                            (pStates,aStates,nameI,prodI)
                            grammarDict.[prod]
                    (tree,pStates0,aStates0,nameI0,prodI0)
                    )
                (new Dictionary<string,State>(),[],[],1,0)
                prodOrder
        (nfaTree,prodStates,accStates,grammarDict,prodOrder)
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
                let (stateName,eClosure,transitions) = dfaTree.[i]
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
    let dfa2gotoTable (dfaTree : (string * SortedDictionary<string,State> * Dictionary<string,string>) []) =
        let translateField = function
            | "Ø" -> "N"
            | s -> sprintf "S %s" s
        Array.init
            dfaTree.Length
            (fun i ->
                let (stateName,eClosure,transitions) = dfaTree.[i]
                Dict<string,string>.fold
                    (fun (row : Dictionary<string,string>) trans ->
                        let (isNonTerm,rest) = isNT trans.Key
                        if isNonTerm then
                            let field = translateField trans.Value
                            row.Add(rest,field)
                            row
                        else
                            row
                        )
                    (new Dictionary<string,string>())
                    transitions
                )
    let dfa2reduceTable (dfaTree : (string * SortedDictionary<string,State> * Dictionary<string,string>) []) (grammar,unfoldedGrammar,nfaAccStates) =
        let nullable = Grammar2Set.calcNullable grammar
        let first = Grammar2Set.calcFirst grammar nullable
        let follow = Grammar2Set.calcFollow grammar first nullable
        
        let tempL = List.length unfoldedGrammar

        for (stateName,eClosure,transitions) in dfaTree do
            printf "accepting(%s) = " stateName
            for (accState,p_i) in nfaAccStates do
                if eClosure.ContainsKey(accState) then
                    printf "{%s}, %s.p = %d, prod(%d) = %A" accState accState p_i p_i unfoldedGrammar.[p_i]
            printfn "" 
        "return"
    let printTable name (table: Dictionary<string,string>[]) =
        printfn "%s" name
        printf "%-10s" "DFA-state"
        for f in table.[0] do
            printf "%-8s" f.Key
        printfn ""
        let mutable i10 = 0
        for row in table do
            printf "s%-10d" i10
            i10 <- i10 + 1
            for f in row do
                printf "%-8s" f.Value 
            printfn ""
    let createTables (grammar : Dictionary<string,(ProdExp list) list>,prodOrder) =
        let prodOrder =
            match prodOrder with
            | [] ->
                grammar.Add("__",[[]])
                ["__"]
            | head::pOrder ->
                grammar.Add("__",[[NonTerm head]])
                "__"::head::pOrder
        let unfoldedGrammar =
            let retval = 
                List.fold
                    (fun acc prod ->
                        List.fold (fun acc prodE -> (prod,prodE)::acc) acc grammar.[prod]
                        )
                    []
                    prodOrder
            List.rev retval
        //let (nfa,nfaProdStates,nfaAccStates,newGrammar,newProdOrder) = grammar2nfa (grammar,prodOrder)
        let (nfa,nfaProdStates,nfaAccStates) = grammar2nfa unfoldedGrammar
        let nfa_eps = nfa2nfa_eps (nfa,nfaProdStates)
        let dfa = nfa2dfa nfa_eps
        let actionTable = dfa2actionTable dfa
        let gotoTable = dfa2gotoTable dfa
        let reduceTable = dfa2reduceTable dfa (grammar,unfoldedGrammar,nfaAccStates)
        
        printTable "action-table" actionTable
        printTable "goto-table" gotoTable

        for trans in nfa do
            let (symbol,target) =
                match trans.Value.transitions with
                | [Char (s,t)] -> (s,t)
                | g -> ("Ø","Ø")
            printfn "(%s) -%A>(%A)" trans.Key symbol target

        "return"
