namespace Maskiner
module GrammarParser =
    open System.Collections.Generic
    open System.Text.RegularExpressions
    type Token =
        | TokenNonTerm of string
        | TokenTerm of string
        | TokenMid
        | TokenProd
        | TokenRightarrow
        | TokenDollar
    type MatchError =
        | ProdError of Token * Token * Token 
        | SymbolError of Token
    type ProdExp =
        | NonTerm of string
        | Term of string
        | Dollar
    let lexer str =
        let tokensL = new List<Token>()
        let addToken (tGroups : GroupCollection) =
            if tGroups.[1].Value <> "" then
                tokensL.Add(TokenProd)
            elif tGroups.[2].Value <> "" then
                tokensL.Add(TokenRightarrow)
            elif tGroups.[3].Value <> "" then
                tokensL.Add(TokenMid)
            elif tGroups.[4].Value <> "" then
                tokensL.Add(TokenNonTerm tGroups.[4].Value)
            elif tGroups.[5].Value <> "" then
                tokensL.Add(TokenTerm tGroups.[5].Value)
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
            tokensL.Add(TokenDollar)
            Array.init tokensL.Count (fun i -> tokensL.[i]) 
    let parser tokens =
        let len = Array.length tokens
        let tree = new Dictionary<string,(ProdExp list) list>()
        let token2str = function
            | TokenNonTerm name -> name
            | TokenTerm name -> "\"" + name + "\""
            | TokenProd -> "prod"
            | TokenRightarrow -> "->"
            | TokenMid -> "|"
            | TokenDollar -> "$"
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
            | TokenProd when mLen 3 ->
                match (tokens.[i],tokens.[i+1],tokens.[i+2]) with
                | (TokenProd,TokenNonTerm name,TokenRightarrow) ->
                    (i+3,tokens.[i+3],name)
                | (a,b,c) -> errorM (ProdError (a,b,c))
            | TokenMid when mLen 1 ->
                (i+1,tokens.[i+1],"")
            | TokenNonTerm name when mLen 1 ->
                (i+1,tokens.[i+1],name)
            | TokenTerm name when mLen 1 ->
                (i+1,tokens.[i+1],name)
            | TokenDollar ->
                (len-1,tokens.[len-1],"")
            | _ -> errorM (SymbolError input)
        let rec parseS i input pOrder =
            match input with
            | TokenProd -> 
                let (i,input,_,pOrder) = parseProd i input pOrder
                let (_,_,_) = matchF i TokenDollar
                pOrder
            | _ -> errorR input
        and parseProd i input pOrder =
            match input with
            | TokenProd ->
                let (i,input,prodName) = matchF i input
                parseProdRight i input (add2tree prodName) (add2order pOrder prodName)
            | _ -> errorR input
        and parseProdRight i input partF pOrder =
            match input with
            | TokenProd | TokenNonTerm _ | TokenTerm _ | TokenMid | TokenDollar ->
                let (i,input) = parseRightSide i input partF 
                parseProdRight' i input pOrder
            | _ -> errorR input
        and parseProdRight' i input pOrder =
            match input with
            | TokenProd ->
                parseProd i input pOrder
            | TokenDollar ->
                (i,input,"",pOrder)
            | _ -> errorR input
        and parseRightSide i input partF =
            match input with
            | TokenNonTerm _ | TokenTerm _ | TokenMid | TokenProd | TokenDollar->
                let (i,input,_) = parseRightExp i input partF []
                parseRightSide' i input partF 
            | _ -> errorR input
        and parseRightSide' i input partF =
            match input with
            | TokenMid ->
                let (i,input,_) = matchF i input
                parseRightSide i input partF
            | TokenProd | TokenDollar ->
                (i,input)
            | _ ->
                errorR input
        and parseRightExp i input partF rExps =
            match input with
            | TokenNonTerm _ ->
                let (i,input,nonTerm) = matchF i input
                let add2rExps = rExps @ [NonTerm nonTerm]
                parseRightExp i input partF add2rExps
            | TokenTerm _ ->
                let (i,input,term) = matchF i input
                let add2rExps = rExps @ [Term term]
                parseRightExp i input partF add2rExps
            | TokenProd | TokenDollar | TokenMid ->
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
