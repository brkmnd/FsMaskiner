namespace Maskiner
module GrammarParser =
    open System.Collections.Generic
    open System.Text.RegularExpressions
    type Token =
        | NonTerm of string
        | Term of string
        | Mid
        | Prod
        | Rightarrow
        | Dollar
    type MatchError =
        | ProdError of Token * Token * Token 
        | SymbolError of Token
    let lexer str =
        let tokensL = new List<Token>()
        let addToken (tGroups : GroupCollection) =
            if tGroups.[1].Value <> "" then
                tokensL.Add(Prod)
            elif tGroups.[2].Value <> "" then
                tokensL.Add(Rightarrow)
            elif tGroups.[3].Value <> "" then
                tokensL.Add(Mid)
            elif tGroups.[4].Value <> "" then
                tokensL.Add(NonTerm tGroups.[4].Value)
            elif tGroups.[5].Value <> "" then
                tokensL.Add(Term tGroups.[5].Value)
            else ()
        let regToken =
            "(prod)|"+
            "(->)|"+
            "(\\|)|"+
            "([A-Z][a-zA-Z']*)|"+
            "\"([^\"]+)\"|"+
            "[\\n\\t\\r ]+"
        let mfun (m : Match) =
            addToken m.Groups
            ""
        let residueStr = Regex.Replace(str,regToken,mfun)
        if residueStr <> "" then
            failwith (sprintf "garbage in expression : %c" residueStr.[0])
        else
            tokensL.Add(Dollar)
            Array.init tokensL.Count (fun i -> tokensL.[i]) 
    let parser tokens =
        let len = Array.length tokens
        let tree = new Dictionary<string,(Grammar2Set.ProdExp list) list>()
        let token2str = function
            | NonTerm name -> name
            | Term name -> "\"" + name + "\""
            | Prod -> "prod"
            | Rightarrow -> "->"
            | Mid -> "|"
            | Dollar -> "$"
        let errorM = function
            | ProdError (a,b,c) ->
                let aStr = token2str a
                let bStr = token2str b
                let cStr = token2str c
                let msg =
                    sprintf
                        "syntax error in def prod: '%s %s %s'"
                        aStr
                        bStr
                        cStr
                failwith msg
            | SymbolError a ->
                let aStr = token2str a
                let msg = sprintf "syntax error: '%s'" aStr
                failwith msg
        let errorR input =
            failwith (sprintf "syntax error: '%s'" (token2str input))
        let add2tree pName rightSide =
            if tree.ContainsKey(pName) then
                let newRightSide = tree.[pName] @ [rightSide]
                tree.[pName] <- newRightSide
            else
                tree.Add(pName,[rightSide])
        let add2order orderL pName =
            if List.exists (fun x -> x = pName) orderL then orderL
            else orderL @ [pName]
        let matchF i input =
            let mLen tl = len - i - tl > 0
            match input with
            | Prod when mLen 3 ->
                match (tokens.[i],tokens.[i+1],tokens.[i+2]) with
                | (Prod,NonTerm name,Rightarrow) ->
                    (i+3,tokens.[i+3],name)
                | (a,b,c) -> errorM (ProdError (a,b,c))
            | Mid when mLen 1 ->
                (i+1,tokens.[i+1],"")
            | NonTerm name when mLen 1 ->
                (i+1,tokens.[i+1],name)
            | Term name when mLen 1 ->
                (i+1,tokens.[i+1],name)
            | Dollar ->
                (len-1,tokens.[len-1],"")
            | _ -> errorM (SymbolError input)
        let rec parseS i input pOrder =
            match input with
            | Prod -> 
                let (i,input,_,pOrder) = parseProd i input pOrder
                let (_,_,_) = matchF i Dollar
                pOrder
            | _ -> errorR input
        and parseProd i input pOrder =
            match input with
            | Prod ->
                let (i,input,prodName) = matchF i input
                parseProdRight i input (add2tree prodName) (add2order pOrder prodName)
            | _ -> errorR input
        and parseProdRight i input partF pOrder =
            match input with
            | Prod | NonTerm _ | Term _ | Mid | Dollar ->
                let (i,input) = parseRightSide i input partF 
                parseProdRight' i input pOrder
            | _ -> errorR input
        and parseProdRight' i input pOrder =
            match input with
            | Prod ->
                parseProd i input pOrder
            | Dollar ->
                (i,input,"",pOrder)
            | _ -> errorR input
        and parseRightSide i input partF =
            match input with
            | NonTerm _ | Term _ | Mid | Prod | Dollar->
                let (i,input,_) = parseRightExp i input partF []
                parseRightSide' i input partF 
            | _ -> errorR input
        and parseRightSide' i input partF =
            match input with
            | Mid ->
                let (i,input,_) = matchF i input
                parseRightSide i input partF
            | Prod | Dollar ->
                (i,input)
            | _ ->
                errorR input
        and parseRightExp i input partF rExps =
            match input with
            | NonTerm _ ->
                let (i,input,nonTerm) = matchF i input
                let add2rExps = rExps @ [Grammar2Set.NonTerm nonTerm]
                parseRightExp i input partF add2rExps
            | Term _ ->
                let (i,input,term) = matchF i input
                let add2rExps = rExps @ [Grammar2Set.Term term]
                parseRightExp i input partF add2rExps
            | Prod | Dollar | Mid ->
                partF rExps
                (i,input,"")
            | _ -> errorR input
        if len > 1 then
            let pOrder = parseS 0 tokens.[0] []
            (tree,pOrder)
        else failwith "syntax error: no input"
    let parseGrammar str =
        let lexed = lexer str
        parser lexed
