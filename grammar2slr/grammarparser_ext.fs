namespace Maskiner
module GrammarParserExt =
    open System.Collections.Generic
    open System.Text.RegularExpressions
    type Action =
        | Shift of int
        | Reduce of int
        | Accept
        | Error of string
    type Tree =
        | EmptyTree
        | LeafStr of string
        | LeafId of string
        | LeafNum of string
        | LeafDir of string
        | RSide of GrammarParser.ProdExp list
        | TokenAtt of Tree list
        | TokenAttCap of Tree list
        | TupleArgs of Tree list
        | DataProds of Dictionary<string,(GrammarParser.ProdExp list) list>
        | DataTokens of Dictionary<string,bool * string>
        | DataTokensOrder of List<string>
        | DataBTokens of List<string>
        | DataPrecs of Dictionary<string,int>
        | DataAssocs of Dictionary<string,string>
        | DataGroups of Dictionary<string,string>
    (* Add leaf nodes/tokens here.
     * That is nodes of type token
     * that is to be leafs in the tree
     * *)
    let addToken2tree tree node =
        match node with
        | ("string",Some str,pos) -> (LeafStr str)::tree
        | ("identifier",Some id,pos) -> (LeafId id)::tree
        | ("num",Some n,pos) -> (LeafNum n)::tree
        | _ -> tree
    let data_productions = new Dictionary<string,(GrammarParser.ProdExp list) list>()
    let data_tokens = new Dictionary<string,bool * string>()
    let data_tokens_order = new List<string>()
    let data_btokens = new List<string>()
    let data_assoc = new Dictionary<string,string>()
    let data_prec = new Dictionary<string,int>()
    let data_groups = new Dictionary<string,string>()
    let clear_data () =
        data_productions.Clear()
        data_tokens.Clear()
        data_tokens_order.Clear()
        data_btokens.Clear()
        data_assoc.Clear()
        data_prec.Clear()
        data_groups.Clear()
    (* Initialize tree stack.
     * This is passed on to the production_funs as tree
     * *)
    let initTreeStack = [
        (DataProds data_productions)
        (DataTokens data_tokens)
        (DataTokensOrder data_tokens_order)
        (DataBTokens data_btokens)
        (DataPrecs data_prec)
        (DataAssocs data_assoc)
        (DataGroups data_groups)
        ]
    let data_add2prods pName rSide =
        let dContains = data_productions.ContainsKey(pName)
        let newRSide =
            let d0 =
                if dContains then
                    []::data_productions.[pName]
                else [[]]
            List.fold
                (fun acc x ->
                    let head = List.head acc
                    let tail = List.tail acc
                    match x with
                    | GrammarParser.Delim -> []::head::tail
                    | t -> (head @ [t])::tail
                    )
                d0
                rSide
        if dContains then
            data_productions.[pName] <- newRSide
        else
            data_productions.Add(pName,newRSide)
    let data_add2prec tName lev =
        if data_prec.ContainsKey(tName) then ()
        else
            data_prec.Add(tName,(int) lev)
    let data_add2tokens cap tNames tVals pos =
        let add2order =
            List.fold
                (fun (acc : List<string>) x ->
                    match x with
                    | LeafStr tName -> acc.Add(tName); acc
                    | _ -> acc
                    )
                data_tokens_order
                (List.rev tNames)
        let check tName = data_tokens.ContainsKey(tName)
        if List.length tNames <> List.length tVals then
            let (x,y) = pos
            let msg =
                sprintf
                    "%s(%d,%d): %s"
                    "syntax error"
                    x
                    y
                    "relative assignment on tuples with different length"
            failwith msg
        else
            let d = List.zip tNames tVals
            List.fold
                (fun uAcc x ->
                    match x with
                    | (LeafStr tName,LeafStr tVal) when not (check tName) ->
                        data_tokens.Add(tName,(cap,tVal))
                        uAcc
                    | _ -> uAcc
                    )
                ()
                (List.zip tNames tVals)
    let data_add2assoc dir args =
        List.fold
            (fun uAcc x ->
                match x with
                | LeafStr tName ->
                    if data_assoc.ContainsKey(tName) then
                        uAcc
                    else
                        data_assoc.Add(tName,dir)
                | _ -> uAcc
                )
            ()
            args
    let data_add2groups gName gElms =
        List.fold
            (fun acc x ->
                match x with
                | LeafStr gElm ->
                    if data_groups.ContainsKey(gElm) then
                        data_groups.[gElm] <- gName
                        acc
                    else
                        data_groups.Add(gElm,gName)
                        acc
                | _ -> acc
                )
            ()
            gElms
    let data_add2btokens args =
        List.fold
            (fun u x ->
                match x with
                | LeafStr btoken -> data_btokens.Add(btoken); u
                | _ -> u
                )
            ()
            args
    (* Add handling of productions here.
     * That is insert into tree or outside
     * data-structures.
     * *)
    let productions_fun = [|
        //[0] __ -> Exp 
        (fun tree pOrder pos -> (tree,pOrder))
        //[1] Exp -> prod Identifier rarrow ProdRight Exp 
        (fun tree pOrder pos ->
            match tree with
            | (RSide rSide)::(LeafId pName)::tree ->
                data_add2prods pName rSide
                let newProdOrder =
                    if List.exists (fun x -> x = pName) pOrder then pOrder
                    else pName::pOrder
                (tree,newProdOrder)
            | _ -> (tree,pOrder)
            )
        //[2] Exp -> prec String Num Exp 
        (fun tree pOrder pos ->
            match tree with
            | (LeafNum n)::(LeafStr token)::tree ->
                data_add2prec token n
                (tree,pOrder)
            | _ -> (tree,pOrder)
            )
        //[3] Exp -> assoc String StringTuple Exp 
        (fun tree pOrder pos ->
            match tree with
            | (TupleArgs args)::(LeafStr dir)::tree ->
                data_add2assoc dir args
                (tree,pOrder)
            | _ -> (tree,pOrder)
            )
        //[4] Exp -> token Token Exp 
        (fun tree pOrder pos ->
            match tree with
            | (TokenAtt [TupleArgs tNames;TupleArgs tVals])::tree ->
                data_add2tokens false tNames tVals pos
                (tree,pOrder)
            | (TokenAttCap [TupleArgs tNames;TupleArgs tVals])::tree ->
                data_add2tokens true tNames tVals pos
                (tree,pOrder)
            | _ -> (tree,pOrder)
            )
        //[5] Exp -> bang token BangToken Exp 
        (fun tree pOrder pos -> (tree,pOrder))
        //[6] Exp -> tgroup Group Exp 
        (fun tree pOrder pos -> (tree,pOrder))
        //[7] Exp -> 
        (fun tree pOrder pos ->
            match tree with
            | _ -> (tree,pOrder)
            )
        //[8] ProdRight -> String ProdRight 
        (fun tree pOrder pos ->
            match tree with
            | (RSide rSide)::(LeafStr term)::tree ->
                ((RSide ((GrammarParser.Term term)::rSide))::tree,pOrder)
            | (LeafStr term)::tree -> ((RSide [GrammarParser.Term term])::tree,pOrder)
            | _ -> (tree,pOrder)
            )
        //[9] ProdRight -> Identifier ProdRight 
        (fun tree pOrder pos ->
            match tree with
            | (RSide rSide)::(LeafId nt)::tree ->
                ((RSide ((GrammarParser.NonTerm nt)::rSide))::tree,pOrder)
            | (LeafId nt)::tree -> ((RSide [GrammarParser.NonTerm nt])::tree,pOrder)
            | _ -> (tree,pOrder)
            )
        //[10] ProdRight -> mid ProdRight 
        (fun tree pOrder pos ->
            match tree with
            | (RSide rSide)::tree ->
                ((RSide (GrammarParser.Delim::rSide))::tree,pOrder)
            | _ -> (tree,pOrder)
            )
        //[11] ProdRight -> 
        (fun tree pOrder pos -> ((RSide [])::tree,pOrder))
        //[12] Token -> StringTuple as StringTuple 
        (fun tree pOrder pos ->
            match tree with
            | tVals::tNames::tree ->
                ((TokenAtt [tNames;tVals])::tree,pOrder)
            | _ -> (tree,pOrder)
            )
        //[13] Token -> f_cap StringTuple as StringTuple
        (fun tree pOrder pos ->
            match tree with
            | tVals::tNames::tree ->
                ((TokenAttCap [tNames;tVals])::tree,pOrder)
            | _ -> (tree,pOrder)
            )
        //[14] BangToken -> StringTuple 
        (fun tree pOrder pos ->
            match tree with
            | (TupleArgs args)::tree ->
                data_add2btokens args
                (tree,pOrder)
            | _ -> (tree,pOrder)
            )
        //[15] Group -> String lbrace StringTuple rbrace 
        (fun tree pOrder pos ->
            match tree with
            | (TupleArgs gElms)::(LeafStr gName)::tree ->
                data_add2groups gName gElms
                (tree,pOrder)
            | _ -> (tree,pOrder)
            )
        //[16] Identifier -> identifier 
        (fun tree pOrder pos -> (tree,pOrder))
        //[17] StringTuple -> String comma StringTuple 
        (fun tree pOrder pos ->
            match tree with
            | (TupleArgs args)::arg::tree ->
                ((TupleArgs (arg::args))::tree,pOrder)
            | arg::tree ->
                ((TupleArgs [arg])::tree,pOrder)
            | _ -> (tree,pOrder)
            )
        //[18] StringTuple -> String 
        (fun tree pOrder pos ->
            match tree with
            | (TupleArgs args)::arg::tree ->
                ((TupleArgs (arg::args))::tree,pOrder)
            | arg::tree ->
                ((TupleArgs [arg])::tree,pOrder)
            | _  -> (tree,pOrder)
            )
        //[19] String -> string 
        (fun tree pOrder pos -> (tree,pOrder))
        //[20] Num -> num 
        (fun tree pOrder pos -> (tree,pOrder))
        |]
    let lexer inStr =
        let tokensL = new List<string * Option<string> * (int * int)>()
        let lineLens = new Stack<int>()
        lineLens.Push(0)
        let addToken xIndex (tGroup : GroupCollection) =
            let totalLineLen = lineLens.Peek()
            if tGroup.[1].Value <> "" then
                tokensL.Add(("assoc",None,(xIndex + 1 - totalLineLen,lineLens.Count)))
            if tGroup.[2].Value <> "" then
                tokensL.Add(("as",None,(xIndex + 1 - totalLineLen,lineLens.Count)))
            if tGroup.[3].Value <> "" then
                tokensL.Add(("bang",None,(xIndex + 1 - totalLineLen,lineLens.Count)))
            if tGroup.[4].Value <> "" then
                tokensL.Add(("colon",None,(xIndex + 1 - totalLineLen,lineLens.Count)))
            if tGroup.[5].Value <> "" then
                tokensL.Add(("comma",None,(xIndex + 1 - totalLineLen,lineLens.Count)))
            if tGroup.[6].Value <> "" then
                tokensL.Add(("group",None,(xIndex + 1 - totalLineLen,lineLens.Count)))
            if tGroup.[7].Value <> "" then
                tokensL.Add(("identifier",Some tGroup.[7].Value,(xIndex + 1 - totalLineLen,lineLens.Count)))
            if tGroup.[8].Value <> "" then
                tokensL.Add(("rbrace",None,(xIndex + 1 - totalLineLen,lineLens.Count)))
            if tGroup.[9].Value <> "" then
                tokensL.Add(("mid",None,(xIndex + 1 - totalLineLen,lineLens.Count)))
            if tGroup.[10].Value <> "" then
                tokensL.Add(("num",Some tGroup.[10].Value,(xIndex + 1 - totalLineLen,lineLens.Count)))
            if tGroup.[11].Value <> "" then
                tokensL.Add(("prec",None,(xIndex + 1 - totalLineLen,lineLens.Count)))
            if tGroup.[12].Value <> "" then
                tokensL.Add(("prod",None,(xIndex + 1 - totalLineLen,lineLens.Count)))
            if tGroup.[13].Value <> "" then
                tokensL.Add(("rarrow",None,(xIndex + 1 - totalLineLen,lineLens.Count)))
            if tGroup.[14].Value <> "" then
                tokensL.Add(("lbrace",None,(xIndex + 1 - totalLineLen,lineLens.Count)))
            if tGroup.[15].Value <> "" then
                tokensL.Add(("string",Some tGroup.[15].Value,(xIndex + 1 - totalLineLen,lineLens.Count)))
            if tGroup.[16].Value <> "" then
                tokensL.Add(("token",None,(xIndex + 1 - totalLineLen,lineLens.Count)))
            if tGroup.[17].Value <> "" then
                tokensL.Add(("f_cap",None,(xIndex + 1 - totalLineLen,lineLens.Count)))
            if tGroup.[18].Value <> "" then
                lineLens.Push(xIndex + 1)
        let regToken =
                "(assoc)|"+
                "(as)|"+
                "(!)|"+
                "(:)|"+
                "(,)|"+
                "(group)|"+
                "([A-Z][a-zA-Z']*)|"+
                "(\\})|"+
                "(\\|)|"+
                "([0-9]+)|"+
                "(prec)|"+
                "(prod)|"+
                "(->)|"+
                "(\\{)|"+
                "\"([^\"]*)\"|"+
                "(token)|"+
                "(-cap)|"+
                "(\\n)|"+
                "[\\t\\r ]+|"+
                "#[^\\n]*\\n"
        let mfun (m : Match) =
            addToken m.Index m.Groups
            ""
        let residueStr = Regex.Replace(inStr,regToken,mfun)
        if residueStr <> "" then
            failwith (sprintf "garbage in expression: %c" residueStr.[0])
        else
            tokensL.Add("$",None,(inStr.Length - 1,lineLens.Count))
            Array.init tokensL.Count (fun i -> tokensL.[i])
    let actionTable = [|
        //s0
        (fun (dict : Dictionary<string,Action>) ->
            dict.Add("as",Error "expected 'definition','token','eoi', but given 'as'")
            dict.Add("assoc",Shift 2)
            dict.Add("bang",Shift 3)
            dict.Add("colon",Error "expected 'definition','token','eoi', but given 'colon'")
            dict.Add("comma",Error "expected 'definition','token','eoi', but given 'comma'")
            dict.Add("f_cap",Error "expected 'definition','token','eoi', but given 'flag'")
            dict.Add("group",Shift 4)
            dict.Add("identifier",Error "expected 'definition','token','eoi', but given 'identifier'")
            dict.Add("lbrace",Error "expected 'definition','token','eoi', but given 'parentheses'")
            dict.Add("mid",Error "expected 'definition','token','eoi', but given 'mid'")
            dict.Add("num",Error "expected 'definition','token','eoi', but given 'literal'")
            dict.Add("prec",Shift 5)
            dict.Add("prod",Shift 6)
            dict.Add("rarrow",Error "expected 'definition','token','eoi', but given 'rarrow'")
            dict.Add("rbrace",Error "expected 'definition','token','eoi', but given 'parentheses'")
            dict.Add("string",Error "expected 'definition','token','eoi', but given 'literal'")
            dict.Add("token",Shift 7)
            dict.Add("$",Reduce 7)
            dict
            )(new Dictionary<string,Action>())
        //s1
        (fun (dict : Dictionary<string,Action>) ->
            dict.Add("as",Error "expected 'eoi', but given 'as'")
            dict.Add("assoc",Error "expected 'eoi', but given 'definition'")
            dict.Add("bang",Error "expected 'eoi', but given 'definition'")
            dict.Add("colon",Error "expected 'eoi', but given 'colon'")
            dict.Add("comma",Error "expected 'eoi', but given 'comma'")
            dict.Add("f_cap",Error "expected 'eoi', but given 'flag'")
            dict.Add("group",Error "expected 'eoi', but given 'definition'")
            dict.Add("identifier",Error "expected 'eoi', but given 'identifier'")
            dict.Add("lbrace",Error "expected 'eoi', but given 'parentheses'")
            dict.Add("mid",Error "expected 'eoi', but given 'mid'")
            dict.Add("num",Error "expected 'eoi', but given 'literal'")
            dict.Add("prec",Error "expected 'eoi', but given 'definition'")
            dict.Add("prod",Error "expected 'eoi', but given 'definition'")
            dict.Add("rarrow",Error "expected 'eoi', but given 'rarrow'")
            dict.Add("rbrace",Error "expected 'eoi', but given 'parentheses'")
            dict.Add("string",Error "expected 'eoi', but given 'literal'")
            dict.Add("token",Error "expected 'eoi', but given 'token'")
            dict.Add("$",Accept)
            dict
            )(new Dictionary<string,Action>())
        //s2
        (fun (dict : Dictionary<string,Action>) ->
            dict.Add("as",Error "expected 'literal', but given 'as'")
            dict.Add("assoc",Error "expected 'literal', but given 'definition'")
            dict.Add("bang",Error "expected 'literal', but given 'definition'")
            dict.Add("colon",Error "expected 'literal', but given 'colon'")
            dict.Add("comma",Error "expected 'literal', but given 'comma'")
            dict.Add("f_cap",Error "expected 'literal', but given 'flag'")
            dict.Add("group",Error "expected 'literal', but given 'definition'")
            dict.Add("identifier",Error "expected 'literal', but given 'identifier'")
            dict.Add("lbrace",Error "expected 'literal', but given 'parentheses'")
            dict.Add("mid",Error "expected 'literal', but given 'mid'")
            dict.Add("num",Error "expected 'literal', but given 'literal'")
            dict.Add("prec",Error "expected 'literal', but given 'definition'")
            dict.Add("prod",Error "expected 'literal', but given 'definition'")
            dict.Add("rarrow",Error "expected 'literal', but given 'rarrow'")
            dict.Add("rbrace",Error "expected 'literal', but given 'parentheses'")
            dict.Add("string",Shift 9)
            dict.Add("token",Error "expected 'literal', but given 'token'")
            dict.Add("$",Error "expected 'literal', but given '$'")
            dict
            )(new Dictionary<string,Action>())
        //s3
        (fun (dict : Dictionary<string,Action>) ->
            dict.Add("as",Error "expected 'token', but given 'as'")
            dict.Add("assoc",Error "expected 'token', but given 'definition'")
            dict.Add("bang",Error "expected 'token', but given 'definition'")
            dict.Add("colon",Error "expected 'token', but given 'colon'")
            dict.Add("comma",Error "expected 'token', but given 'comma'")
            dict.Add("f_cap",Error "expected 'token', but given 'flag'")
            dict.Add("group",Error "expected 'token', but given 'definition'")
            dict.Add("identifier",Error "expected 'token', but given 'identifier'")
            dict.Add("lbrace",Error "expected 'token', but given 'parentheses'")
            dict.Add("mid",Error "expected 'token', but given 'mid'")
            dict.Add("num",Error "expected 'token', but given 'literal'")
            dict.Add("prec",Error "expected 'token', but given 'definition'")
            dict.Add("prod",Error "expected 'token', but given 'definition'")
            dict.Add("rarrow",Error "expected 'token', but given 'rarrow'")
            dict.Add("rbrace",Error "expected 'token', but given 'parentheses'")
            dict.Add("string",Error "expected 'token', but given 'literal'")
            dict.Add("token",Shift 10)
            dict.Add("$",Error "expected 'token', but given '$'")
            dict
            )(new Dictionary<string,Action>())
        //s4
        (fun (dict : Dictionary<string,Action>) ->
            dict.Add("as",Error "expected 'literal', but given 'as'")
            dict.Add("assoc",Error "expected 'literal', but given 'definition'")
            dict.Add("bang",Error "expected 'literal', but given 'definition'")
            dict.Add("colon",Error "expected 'literal', but given 'colon'")
            dict.Add("comma",Error "expected 'literal', but given 'comma'")
            dict.Add("f_cap",Error "expected 'literal', but given 'flag'")
            dict.Add("group",Error "expected 'literal', but given 'definition'")
            dict.Add("identifier",Error "expected 'literal', but given 'identifier'")
            dict.Add("lbrace",Error "expected 'literal', but given 'parentheses'")
            dict.Add("mid",Error "expected 'literal', but given 'mid'")
            dict.Add("num",Error "expected 'literal', but given 'literal'")
            dict.Add("prec",Error "expected 'literal', but given 'definition'")
            dict.Add("prod",Error "expected 'literal', but given 'definition'")
            dict.Add("rarrow",Error "expected 'literal', but given 'rarrow'")
            dict.Add("rbrace",Error "expected 'literal', but given 'parentheses'")
            dict.Add("string",Shift 9)
            dict.Add("token",Error "expected 'literal', but given 'token'")
            dict.Add("$",Error "expected 'literal', but given '$'")
            dict
            )(new Dictionary<string,Action>())
        //s5
        (fun (dict : Dictionary<string,Action>) ->
            dict.Add("as",Error "expected 'literal', but given 'as'")
            dict.Add("assoc",Error "expected 'literal', but given 'definition'")
            dict.Add("bang",Error "expected 'literal', but given 'definition'")
            dict.Add("colon",Error "expected 'literal', but given 'colon'")
            dict.Add("comma",Error "expected 'literal', but given 'comma'")
            dict.Add("f_cap",Error "expected 'literal', but given 'flag'")
            dict.Add("group",Error "expected 'literal', but given 'definition'")
            dict.Add("identifier",Error "expected 'literal', but given 'identifier'")
            dict.Add("lbrace",Error "expected 'literal', but given 'parentheses'")
            dict.Add("mid",Error "expected 'literal', but given 'mid'")
            dict.Add("num",Error "expected 'literal', but given 'literal'")
            dict.Add("prec",Error "expected 'literal', but given 'definition'")
            dict.Add("prod",Error "expected 'literal', but given 'definition'")
            dict.Add("rarrow",Error "expected 'literal', but given 'rarrow'")
            dict.Add("rbrace",Error "expected 'literal', but given 'parentheses'")
            dict.Add("string",Shift 9)
            dict.Add("token",Error "expected 'literal', but given 'token'")
            dict.Add("$",Error "expected 'literal', but given '$'")
            dict
            )(new Dictionary<string,Action>())
        //s6
        (fun (dict : Dictionary<string,Action>) ->
            dict.Add("as",Error "expected 'identifier', but given 'as'")
            dict.Add("assoc",Error "expected 'identifier', but given 'definition'")
            dict.Add("bang",Error "expected 'identifier', but given 'definition'")
            dict.Add("colon",Error "expected 'identifier', but given 'colon'")
            dict.Add("comma",Error "expected 'identifier', but given 'comma'")
            dict.Add("f_cap",Error "expected 'identifier', but given 'flag'")
            dict.Add("group",Error "expected 'identifier', but given 'definition'")
            dict.Add("identifier",Shift 15)
            dict.Add("lbrace",Error "expected 'identifier', but given 'parentheses'")
            dict.Add("mid",Error "expected 'identifier', but given 'mid'")
            dict.Add("num",Error "expected 'identifier', but given 'literal'")
            dict.Add("prec",Error "expected 'identifier', but given 'definition'")
            dict.Add("prod",Error "expected 'identifier', but given 'definition'")
            dict.Add("rarrow",Error "expected 'identifier', but given 'rarrow'")
            dict.Add("rbrace",Error "expected 'identifier', but given 'parentheses'")
            dict.Add("string",Error "expected 'identifier', but given 'literal'")
            dict.Add("token",Error "expected 'identifier', but given 'token'")
            dict.Add("$",Error "expected 'identifier', but given '$'")
            dict
            )(new Dictionary<string,Action>())
        //s7
        (fun (dict : Dictionary<string,Action>) ->
            dict.Add("as",Error "expected 'flag','literal', but given 'as'")
            dict.Add("assoc",Error "expected 'flag','literal', but given 'definition'")
            dict.Add("bang",Error "expected 'flag','literal', but given 'definition'")
            dict.Add("colon",Error "expected 'flag','literal', but given 'colon'")
            dict.Add("comma",Error "expected 'flag','literal', but given 'comma'")
            dict.Add("f_cap",Shift 19)
            dict.Add("group",Error "expected 'flag','literal', but given 'definition'")
            dict.Add("identifier",Error "expected 'flag','literal', but given 'identifier'")
            dict.Add("lbrace",Error "expected 'flag','literal', but given 'parentheses'")
            dict.Add("mid",Error "expected 'flag','literal', but given 'mid'")
            dict.Add("num",Error "expected 'flag','literal', but given 'literal'")
            dict.Add("prec",Error "expected 'flag','literal', but given 'definition'")
            dict.Add("prod",Error "expected 'flag','literal', but given 'definition'")
            dict.Add("rarrow",Error "expected 'flag','literal', but given 'rarrow'")
            dict.Add("rbrace",Error "expected 'flag','literal', but given 'parentheses'")
            dict.Add("string",Shift 9)
            dict.Add("token",Error "expected 'flag','literal', but given 'token'")
            dict.Add("$",Error "expected 'flag','literal', but given '$'")
            dict
            )(new Dictionary<string,Action>())
        //s8
        (fun (dict : Dictionary<string,Action>) ->
            dict.Add("as",Error "expected 'colon', but given 'as'")
            dict.Add("assoc",Error "expected 'colon', but given 'definition'")
            dict.Add("bang",Error "expected 'colon', but given 'definition'")
            dict.Add("colon",Shift 20)
            dict.Add("comma",Error "expected 'colon', but given 'comma'")
            dict.Add("f_cap",Error "expected 'colon', but given 'flag'")
            dict.Add("group",Error "expected 'colon', but given 'definition'")
            dict.Add("identifier",Error "expected 'colon', but given 'identifier'")
            dict.Add("lbrace",Error "expected 'colon', but given 'parentheses'")
            dict.Add("mid",Error "expected 'colon', but given 'mid'")
            dict.Add("num",Error "expected 'colon', but given 'literal'")
            dict.Add("prec",Error "expected 'colon', but given 'definition'")
            dict.Add("prod",Error "expected 'colon', but given 'definition'")
            dict.Add("rarrow",Error "expected 'colon', but given 'rarrow'")
            dict.Add("rbrace",Error "expected 'colon', but given 'parentheses'")
            dict.Add("string",Error "expected 'colon', but given 'literal'")
            dict.Add("token",Error "expected 'colon', but given 'token'")
            dict.Add("$",Error "expected 'colon', but given '$'")
            dict
            )(new Dictionary<string,Action>())
        //s9
        (fun (dict : Dictionary<string,Action>) ->
            dict.Add("as",Reduce 19)
            dict.Add("assoc",Reduce 19)
            dict.Add("bang",Reduce 19)
            dict.Add("colon",Reduce 19)
            dict.Add("comma",Reduce 19)
            dict.Add("f_cap",Error "expected 'as','definition','colon','comma','identifier','parentheses','mid','literal','token','eoi', but given 'flag'")
            dict.Add("group",Reduce 19)
            dict.Add("identifier",Reduce 19)
            dict.Add("lbrace",Reduce 19)
            dict.Add("mid",Reduce 19)
            dict.Add("num",Reduce 19)
            dict.Add("prec",Reduce 19)
            dict.Add("prod",Reduce 19)
            dict.Add("rarrow",Error "expected 'as','definition','colon','comma','identifier','parentheses','mid','literal','token','eoi', but given 'rarrow'")
            dict.Add("rbrace",Reduce 19)
            dict.Add("string",Reduce 19)
            dict.Add("token",Reduce 19)
            dict.Add("$",Reduce 19)
            dict
            )(new Dictionary<string,Action>())
        //s10
        (fun (dict : Dictionary<string,Action>) ->
            dict.Add("as",Error "expected 'literal', but given 'as'")
            dict.Add("assoc",Error "expected 'literal', but given 'definition'")
            dict.Add("bang",Error "expected 'literal', but given 'definition'")
            dict.Add("colon",Error "expected 'literal', but given 'colon'")
            dict.Add("comma",Error "expected 'literal', but given 'comma'")
            dict.Add("f_cap",Error "expected 'literal', but given 'flag'")
            dict.Add("group",Error "expected 'literal', but given 'definition'")
            dict.Add("identifier",Error "expected 'literal', but given 'identifier'")
            dict.Add("lbrace",Error "expected 'literal', but given 'parentheses'")
            dict.Add("mid",Error "expected 'literal', but given 'mid'")
            dict.Add("num",Error "expected 'literal', but given 'literal'")
            dict.Add("prec",Error "expected 'literal', but given 'definition'")
            dict.Add("prod",Error "expected 'literal', but given 'definition'")
            dict.Add("rarrow",Error "expected 'literal', but given 'rarrow'")
            dict.Add("rbrace",Error "expected 'literal', but given 'parentheses'")
            dict.Add("string",Shift 9)
            dict.Add("token",Error "expected 'literal', but given 'token'")
            dict.Add("$",Error "expected 'literal', but given '$'")
            dict
            )(new Dictionary<string,Action>())
        //s11
        (fun (dict : Dictionary<string,Action>) ->
            dict.Add("as",Error "expected 'definition','token','eoi', but given 'as'")
            dict.Add("assoc",Shift 2)
            dict.Add("bang",Shift 3)
            dict.Add("colon",Error "expected 'definition','token','eoi', but given 'colon'")
            dict.Add("comma",Error "expected 'definition','token','eoi', but given 'comma'")
            dict.Add("f_cap",Error "expected 'definition','token','eoi', but given 'flag'")
            dict.Add("group",Shift 4)
            dict.Add("identifier",Error "expected 'definition','token','eoi', but given 'identifier'")
            dict.Add("lbrace",Error "expected 'definition','token','eoi', but given 'parentheses'")
            dict.Add("mid",Error "expected 'definition','token','eoi', but given 'mid'")
            dict.Add("num",Error "expected 'definition','token','eoi', but given 'literal'")
            dict.Add("prec",Shift 5)
            dict.Add("prod",Shift 6)
            dict.Add("rarrow",Error "expected 'definition','token','eoi', but given 'rarrow'")
            dict.Add("rbrace",Error "expected 'definition','token','eoi', but given 'parentheses'")
            dict.Add("string",Error "expected 'definition','token','eoi', but given 'literal'")
            dict.Add("token",Shift 7)
            dict.Add("$",Reduce 7)
            dict
            )(new Dictionary<string,Action>())
        //s12
        (fun (dict : Dictionary<string,Action>) ->
            dict.Add("as",Error "expected 'parentheses', but given 'as'")
            dict.Add("assoc",Error "expected 'parentheses', but given 'definition'")
            dict.Add("bang",Error "expected 'parentheses', but given 'definition'")
            dict.Add("colon",Error "expected 'parentheses', but given 'colon'")
            dict.Add("comma",Error "expected 'parentheses', but given 'comma'")
            dict.Add("f_cap",Error "expected 'parentheses', but given 'flag'")
            dict.Add("group",Error "expected 'parentheses', but given 'definition'")
            dict.Add("identifier",Error "expected 'parentheses', but given 'identifier'")
            dict.Add("lbrace",Shift 24)
            dict.Add("mid",Error "expected 'parentheses', but given 'mid'")
            dict.Add("num",Error "expected 'parentheses', but given 'literal'")
            dict.Add("prec",Error "expected 'parentheses', but given 'definition'")
            dict.Add("prod",Error "expected 'parentheses', but given 'definition'")
            dict.Add("rarrow",Error "expected 'parentheses', but given 'rarrow'")
            dict.Add("rbrace",Error "expected 'parentheses', but given 'parentheses'")
            dict.Add("string",Error "expected 'parentheses', but given 'literal'")
            dict.Add("token",Error "expected 'parentheses', but given 'token'")
            dict.Add("$",Error "expected 'parentheses', but given '$'")
            dict
            )(new Dictionary<string,Action>())
        //s13
        (fun (dict : Dictionary<string,Action>) ->
            dict.Add("as",Error "expected 'literal', but given 'as'")
            dict.Add("assoc",Error "expected 'literal', but given 'definition'")
            dict.Add("bang",Error "expected 'literal', but given 'definition'")
            dict.Add("colon",Error "expected 'literal', but given 'colon'")
            dict.Add("comma",Error "expected 'literal', but given 'comma'")
            dict.Add("f_cap",Error "expected 'literal', but given 'flag'")
            dict.Add("group",Error "expected 'literal', but given 'definition'")
            dict.Add("identifier",Error "expected 'literal', but given 'identifier'")
            dict.Add("lbrace",Error "expected 'literal', but given 'parentheses'")
            dict.Add("mid",Error "expected 'literal', but given 'mid'")
            dict.Add("num",Shift 26)
            dict.Add("prec",Error "expected 'literal', but given 'definition'")
            dict.Add("prod",Error "expected 'literal', but given 'definition'")
            dict.Add("rarrow",Error "expected 'literal', but given 'rarrow'")
            dict.Add("rbrace",Error "expected 'literal', but given 'parentheses'")
            dict.Add("string",Error "expected 'literal', but given 'literal'")
            dict.Add("token",Error "expected 'literal', but given 'token'")
            dict.Add("$",Error "expected 'literal', but given '$'")
            dict
            )(new Dictionary<string,Action>())
        //s14
        (fun (dict : Dictionary<string,Action>) ->
            dict.Add("as",Error "expected 'rarrow', but given 'as'")
            dict.Add("assoc",Error "expected 'rarrow', but given 'definition'")
            dict.Add("bang",Error "expected 'rarrow', but given 'definition'")
            dict.Add("colon",Error "expected 'rarrow', but given 'colon'")
            dict.Add("comma",Error "expected 'rarrow', but given 'comma'")
            dict.Add("f_cap",Error "expected 'rarrow', but given 'flag'")
            dict.Add("group",Error "expected 'rarrow', but given 'definition'")
            dict.Add("identifier",Error "expected 'rarrow', but given 'identifier'")
            dict.Add("lbrace",Error "expected 'rarrow', but given 'parentheses'")
            dict.Add("mid",Error "expected 'rarrow', but given 'mid'")
            dict.Add("num",Error "expected 'rarrow', but given 'literal'")
            dict.Add("prec",Error "expected 'rarrow', but given 'definition'")
            dict.Add("prod",Error "expected 'rarrow', but given 'definition'")
            dict.Add("rarrow",Shift 27)
            dict.Add("rbrace",Error "expected 'rarrow', but given 'parentheses'")
            dict.Add("string",Error "expected 'rarrow', but given 'literal'")
            dict.Add("token",Error "expected 'rarrow', but given 'token'")
            dict.Add("$",Error "expected 'rarrow', but given '$'")
            dict
            )(new Dictionary<string,Action>())
        //s15
        (fun (dict : Dictionary<string,Action>) ->
            dict.Add("as",Error "expected 'definition','identifier','mid','rarrow','literal','token','eoi', but given 'as'")
            dict.Add("assoc",Reduce 16)
            dict.Add("bang",Reduce 16)
            dict.Add("colon",Error "expected 'definition','identifier','mid','rarrow','literal','token','eoi', but given 'colon'")
            dict.Add("comma",Error "expected 'definition','identifier','mid','rarrow','literal','token','eoi', but given 'comma'")
            dict.Add("f_cap",Error "expected 'definition','identifier','mid','rarrow','literal','token','eoi', but given 'flag'")
            dict.Add("group",Reduce 16)
            dict.Add("identifier",Reduce 16)
            dict.Add("lbrace",Error "expected 'definition','identifier','mid','rarrow','literal','token','eoi', but given 'parentheses'")
            dict.Add("mid",Reduce 16)
            dict.Add("num",Error "expected 'definition','identifier','mid','rarrow','literal','token','eoi', but given 'literal'")
            dict.Add("prec",Reduce 16)
            dict.Add("prod",Reduce 16)
            dict.Add("rarrow",Reduce 16)
            dict.Add("rbrace",Error "expected 'definition','identifier','mid','rarrow','literal','token','eoi', but given 'parentheses'")
            dict.Add("string",Reduce 16)
            dict.Add("token",Reduce 16)
            dict.Add("$",Reduce 16)
            dict
            )(new Dictionary<string,Action>())
        //s16
        (fun (dict : Dictionary<string,Action>) ->
            dict.Add("as",Reduce 18)
            dict.Add("assoc",Reduce 18)
            dict.Add("bang",Reduce 18)
            dict.Add("colon",Error "expected 'as','definition','comma','parentheses','token','eoi', but given 'colon'")
            dict.Add("comma",Shift 28)
            dict.Add("f_cap",Error "expected 'as','definition','comma','parentheses','token','eoi', but given 'flag'")
            dict.Add("group",Reduce 18)
            dict.Add("identifier",Error "expected 'as','definition','comma','parentheses','token','eoi', but given 'identifier'")
            dict.Add("lbrace",Error "expected 'as','definition','comma','parentheses','token','eoi', but given 'parentheses'")
            dict.Add("mid",Error "expected 'as','definition','comma','parentheses','token','eoi', but given 'mid'")
            dict.Add("num",Error "expected 'as','definition','comma','parentheses','token','eoi', but given 'literal'")
            dict.Add("prec",Reduce 18)
            dict.Add("prod",Reduce 18)
            dict.Add("rarrow",Error "expected 'as','definition','comma','parentheses','token','eoi', but given 'rarrow'")
            dict.Add("rbrace",Reduce 18)
            dict.Add("string",Error "expected 'as','definition','comma','parentheses','token','eoi', but given 'literal'")
            dict.Add("token",Reduce 18)
            dict.Add("$",Reduce 18)
            dict
            )(new Dictionary<string,Action>())
        //s17
        (fun (dict : Dictionary<string,Action>) ->
            dict.Add("as",Shift 29)
            dict.Add("assoc",Error "expected 'as', but given 'definition'")
            dict.Add("bang",Error "expected 'as', but given 'definition'")
            dict.Add("colon",Error "expected 'as', but given 'colon'")
            dict.Add("comma",Error "expected 'as', but given 'comma'")
            dict.Add("f_cap",Error "expected 'as', but given 'flag'")
            dict.Add("group",Error "expected 'as', but given 'definition'")
            dict.Add("identifier",Error "expected 'as', but given 'identifier'")
            dict.Add("lbrace",Error "expected 'as', but given 'parentheses'")
            dict.Add("mid",Error "expected 'as', but given 'mid'")
            dict.Add("num",Error "expected 'as', but given 'literal'")
            dict.Add("prec",Error "expected 'as', but given 'definition'")
            dict.Add("prod",Error "expected 'as', but given 'definition'")
            dict.Add("rarrow",Error "expected 'as', but given 'rarrow'")
            dict.Add("rbrace",Error "expected 'as', but given 'parentheses'")
            dict.Add("string",Error "expected 'as', but given 'literal'")
            dict.Add("token",Error "expected 'as', but given 'token'")
            dict.Add("$",Error "expected 'as', but given '$'")
            dict
            )(new Dictionary<string,Action>())
        //s18
        (fun (dict : Dictionary<string,Action>) ->
            dict.Add("as",Error "expected 'definition','token','eoi', but given 'as'")
            dict.Add("assoc",Shift 2)
            dict.Add("bang",Shift 3)
            dict.Add("colon",Error "expected 'definition','token','eoi', but given 'colon'")
            dict.Add("comma",Error "expected 'definition','token','eoi', but given 'comma'")
            dict.Add("f_cap",Error "expected 'definition','token','eoi', but given 'flag'")
            dict.Add("group",Shift 4)
            dict.Add("identifier",Error "expected 'definition','token','eoi', but given 'identifier'")
            dict.Add("lbrace",Error "expected 'definition','token','eoi', but given 'parentheses'")
            dict.Add("mid",Error "expected 'definition','token','eoi', but given 'mid'")
            dict.Add("num",Error "expected 'definition','token','eoi', but given 'literal'")
            dict.Add("prec",Shift 5)
            dict.Add("prod",Shift 6)
            dict.Add("rarrow",Error "expected 'definition','token','eoi', but given 'rarrow'")
            dict.Add("rbrace",Error "expected 'definition','token','eoi', but given 'parentheses'")
            dict.Add("string",Error "expected 'definition','token','eoi', but given 'literal'")
            dict.Add("token",Shift 7)
            dict.Add("$",Reduce 7)
            dict
            )(new Dictionary<string,Action>())
        //s19
        (fun (dict : Dictionary<string,Action>) ->
            dict.Add("as",Error "expected 'literal', but given 'as'")
            dict.Add("assoc",Error "expected 'literal', but given 'definition'")
            dict.Add("bang",Error "expected 'literal', but given 'definition'")
            dict.Add("colon",Error "expected 'literal', but given 'colon'")
            dict.Add("comma",Error "expected 'literal', but given 'comma'")
            dict.Add("f_cap",Error "expected 'literal', but given 'flag'")
            dict.Add("group",Error "expected 'literal', but given 'definition'")
            dict.Add("identifier",Error "expected 'literal', but given 'identifier'")
            dict.Add("lbrace",Error "expected 'literal', but given 'parentheses'")
            dict.Add("mid",Error "expected 'literal', but given 'mid'")
            dict.Add("num",Error "expected 'literal', but given 'literal'")
            dict.Add("prec",Error "expected 'literal', but given 'definition'")
            dict.Add("prod",Error "expected 'literal', but given 'definition'")
            dict.Add("rarrow",Error "expected 'literal', but given 'rarrow'")
            dict.Add("rbrace",Error "expected 'literal', but given 'parentheses'")
            dict.Add("string",Shift 9)
            dict.Add("token",Error "expected 'literal', but given 'token'")
            dict.Add("$",Error "expected 'literal', but given '$'")
            dict
            )(new Dictionary<string,Action>())
        //s20
        (fun (dict : Dictionary<string,Action>) ->
            dict.Add("as",Error "expected 'literal', but given 'as'")
            dict.Add("assoc",Error "expected 'literal', but given 'definition'")
            dict.Add("bang",Error "expected 'literal', but given 'definition'")
            dict.Add("colon",Error "expected 'literal', but given 'colon'")
            dict.Add("comma",Error "expected 'literal', but given 'comma'")
            dict.Add("f_cap",Error "expected 'literal', but given 'flag'")
            dict.Add("group",Error "expected 'literal', but given 'definition'")
            dict.Add("identifier",Error "expected 'literal', but given 'identifier'")
            dict.Add("lbrace",Error "expected 'literal', but given 'parentheses'")
            dict.Add("mid",Error "expected 'literal', but given 'mid'")
            dict.Add("num",Error "expected 'literal', but given 'literal'")
            dict.Add("prec",Error "expected 'literal', but given 'definition'")
            dict.Add("prod",Error "expected 'literal', but given 'definition'")
            dict.Add("rarrow",Error "expected 'literal', but given 'rarrow'")
            dict.Add("rbrace",Error "expected 'literal', but given 'parentheses'")
            dict.Add("string",Shift 9)
            dict.Add("token",Error "expected 'literal', but given 'token'")
            dict.Add("$",Error "expected 'literal', but given '$'")
            dict
            )(new Dictionary<string,Action>())
        //s21
        (fun (dict : Dictionary<string,Action>) ->
            dict.Add("as",Error "expected 'definition','token','eoi', but given 'as'")
            dict.Add("assoc",Shift 2)
            dict.Add("bang",Shift 3)
            dict.Add("colon",Error "expected 'definition','token','eoi', but given 'colon'")
            dict.Add("comma",Error "expected 'definition','token','eoi', but given 'comma'")
            dict.Add("f_cap",Error "expected 'definition','token','eoi', but given 'flag'")
            dict.Add("group",Shift 4)
            dict.Add("identifier",Error "expected 'definition','token','eoi', but given 'identifier'")
            dict.Add("lbrace",Error "expected 'definition','token','eoi', but given 'parentheses'")
            dict.Add("mid",Error "expected 'definition','token','eoi', but given 'mid'")
            dict.Add("num",Error "expected 'definition','token','eoi', but given 'literal'")
            dict.Add("prec",Shift 5)
            dict.Add("prod",Shift 6)
            dict.Add("rarrow",Error "expected 'definition','token','eoi', but given 'rarrow'")
            dict.Add("rbrace",Error "expected 'definition','token','eoi', but given 'parentheses'")
            dict.Add("string",Error "expected 'definition','token','eoi', but given 'literal'")
            dict.Add("token",Shift 7)
            dict.Add("$",Reduce 7)
            dict
            )(new Dictionary<string,Action>())
        //s22
        (fun (dict : Dictionary<string,Action>) ->
            dict.Add("as",Error "expected 'definition','token','eoi', but given 'as'")
            dict.Add("assoc",Reduce 14)
            dict.Add("bang",Reduce 14)
            dict.Add("colon",Error "expected 'definition','token','eoi', but given 'colon'")
            dict.Add("comma",Error "expected 'definition','token','eoi', but given 'comma'")
            dict.Add("f_cap",Error "expected 'definition','token','eoi', but given 'flag'")
            dict.Add("group",Reduce 14)
            dict.Add("identifier",Error "expected 'definition','token','eoi', but given 'identifier'")
            dict.Add("lbrace",Error "expected 'definition','token','eoi', but given 'parentheses'")
            dict.Add("mid",Error "expected 'definition','token','eoi', but given 'mid'")
            dict.Add("num",Error "expected 'definition','token','eoi', but given 'literal'")
            dict.Add("prec",Reduce 14)
            dict.Add("prod",Reduce 14)
            dict.Add("rarrow",Error "expected 'definition','token','eoi', but given 'rarrow'")
            dict.Add("rbrace",Error "expected 'definition','token','eoi', but given 'parentheses'")
            dict.Add("string",Error "expected 'definition','token','eoi', but given 'literal'")
            dict.Add("token",Reduce 14)
            dict.Add("$",Reduce 14)
            dict
            )(new Dictionary<string,Action>())
        //s23
        (fun (dict : Dictionary<string,Action>) ->
            dict.Add("as",Error "expected 'eoi', but given 'as'")
            dict.Add("assoc",Error "expected 'eoi', but given 'definition'")
            dict.Add("bang",Error "expected 'eoi', but given 'definition'")
            dict.Add("colon",Error "expected 'eoi', but given 'colon'")
            dict.Add("comma",Error "expected 'eoi', but given 'comma'")
            dict.Add("f_cap",Error "expected 'eoi', but given 'flag'")
            dict.Add("group",Error "expected 'eoi', but given 'definition'")
            dict.Add("identifier",Error "expected 'eoi', but given 'identifier'")
            dict.Add("lbrace",Error "expected 'eoi', but given 'parentheses'")
            dict.Add("mid",Error "expected 'eoi', but given 'mid'")
            dict.Add("num",Error "expected 'eoi', but given 'literal'")
            dict.Add("prec",Error "expected 'eoi', but given 'definition'")
            dict.Add("prod",Error "expected 'eoi', but given 'definition'")
            dict.Add("rarrow",Error "expected 'eoi', but given 'rarrow'")
            dict.Add("rbrace",Error "expected 'eoi', but given 'parentheses'")
            dict.Add("string",Error "expected 'eoi', but given 'literal'")
            dict.Add("token",Error "expected 'eoi', but given 'token'")
            dict.Add("$",Reduce 6)
            dict
            )(new Dictionary<string,Action>())
        //s24
        (fun (dict : Dictionary<string,Action>) ->
            dict.Add("as",Error "expected 'literal', but given 'as'")
            dict.Add("assoc",Error "expected 'literal', but given 'definition'")
            dict.Add("bang",Error "expected 'literal', but given 'definition'")
            dict.Add("colon",Error "expected 'literal', but given 'colon'")
            dict.Add("comma",Error "expected 'literal', but given 'comma'")
            dict.Add("f_cap",Error "expected 'literal', but given 'flag'")
            dict.Add("group",Error "expected 'literal', but given 'definition'")
            dict.Add("identifier",Error "expected 'literal', but given 'identifier'")
            dict.Add("lbrace",Error "expected 'literal', but given 'parentheses'")
            dict.Add("mid",Error "expected 'literal', but given 'mid'")
            dict.Add("num",Error "expected 'literal', but given 'literal'")
            dict.Add("prec",Error "expected 'literal', but given 'definition'")
            dict.Add("prod",Error "expected 'literal', but given 'definition'")
            dict.Add("rarrow",Error "expected 'literal', but given 'rarrow'")
            dict.Add("rbrace",Error "expected 'literal', but given 'parentheses'")
            dict.Add("string",Shift 9)
            dict.Add("token",Error "expected 'literal', but given 'token'")
            dict.Add("$",Error "expected 'literal', but given '$'")
            dict
            )(new Dictionary<string,Action>())
        //s25
        (fun (dict : Dictionary<string,Action>) ->
            dict.Add("as",Error "expected 'definition','token','eoi', but given 'as'")
            dict.Add("assoc",Shift 2)
            dict.Add("bang",Shift 3)
            dict.Add("colon",Error "expected 'definition','token','eoi', but given 'colon'")
            dict.Add("comma",Error "expected 'definition','token','eoi', but given 'comma'")
            dict.Add("f_cap",Error "expected 'definition','token','eoi', but given 'flag'")
            dict.Add("group",Shift 4)
            dict.Add("identifier",Error "expected 'definition','token','eoi', but given 'identifier'")
            dict.Add("lbrace",Error "expected 'definition','token','eoi', but given 'parentheses'")
            dict.Add("mid",Error "expected 'definition','token','eoi', but given 'mid'")
            dict.Add("num",Error "expected 'definition','token','eoi', but given 'literal'")
            dict.Add("prec",Shift 5)
            dict.Add("prod",Shift 6)
            dict.Add("rarrow",Error "expected 'definition','token','eoi', but given 'rarrow'")
            dict.Add("rbrace",Error "expected 'definition','token','eoi', but given 'parentheses'")
            dict.Add("string",Error "expected 'definition','token','eoi', but given 'literal'")
            dict.Add("token",Shift 7)
            dict.Add("$",Reduce 7)
            dict
            )(new Dictionary<string,Action>())
        //s26
        (fun (dict : Dictionary<string,Action>) ->
            dict.Add("as",Error "expected 'definition','token','eoi', but given 'as'")
            dict.Add("assoc",Reduce 20)
            dict.Add("bang",Reduce 20)
            dict.Add("colon",Error "expected 'definition','token','eoi', but given 'colon'")
            dict.Add("comma",Error "expected 'definition','token','eoi', but given 'comma'")
            dict.Add("f_cap",Error "expected 'definition','token','eoi', but given 'flag'")
            dict.Add("group",Reduce 20)
            dict.Add("identifier",Error "expected 'definition','token','eoi', but given 'identifier'")
            dict.Add("lbrace",Error "expected 'definition','token','eoi', but given 'parentheses'")
            dict.Add("mid",Error "expected 'definition','token','eoi', but given 'mid'")
            dict.Add("num",Error "expected 'definition','token','eoi', but given 'literal'")
            dict.Add("prec",Reduce 20)
            dict.Add("prod",Reduce 20)
            dict.Add("rarrow",Error "expected 'definition','token','eoi', but given 'rarrow'")
            dict.Add("rbrace",Error "expected 'definition','token','eoi', but given 'parentheses'")
            dict.Add("string",Error "expected 'definition','token','eoi', but given 'literal'")
            dict.Add("token",Reduce 20)
            dict.Add("$",Reduce 20)
            dict
            )(new Dictionary<string,Action>())
        //s27
        (fun (dict : Dictionary<string,Action>) ->
            dict.Add("as",Error "expected 'definition','identifier','mid','literal','token','eoi', but given 'as'")
            dict.Add("assoc",Reduce 11)
            dict.Add("bang",Reduce 11)
            dict.Add("colon",Error "expected 'definition','identifier','mid','literal','token','eoi', but given 'colon'")
            dict.Add("comma",Error "expected 'definition','identifier','mid','literal','token','eoi', but given 'comma'")
            dict.Add("f_cap",Error "expected 'definition','identifier','mid','literal','token','eoi', but given 'flag'")
            dict.Add("group",Reduce 11)
            dict.Add("identifier",Shift 15)
            dict.Add("lbrace",Error "expected 'definition','identifier','mid','literal','token','eoi', but given 'parentheses'")
            dict.Add("mid",Shift 39)
            dict.Add("num",Error "expected 'definition','identifier','mid','literal','token','eoi', but given 'literal'")
            dict.Add("prec",Reduce 11)
            dict.Add("prod",Reduce 11)
            dict.Add("rarrow",Error "expected 'definition','identifier','mid','literal','token','eoi', but given 'rarrow'")
            dict.Add("rbrace",Error "expected 'definition','identifier','mid','literal','token','eoi', but given 'parentheses'")
            dict.Add("string",Shift 9)
            dict.Add("token",Reduce 11)
            dict.Add("$",Reduce 11)
            dict
            )(new Dictionary<string,Action>())
        //s28
        (fun (dict : Dictionary<string,Action>) ->
            dict.Add("as",Error "expected 'literal', but given 'as'")
            dict.Add("assoc",Error "expected 'literal', but given 'definition'")
            dict.Add("bang",Error "expected 'literal', but given 'definition'")
            dict.Add("colon",Error "expected 'literal', but given 'colon'")
            dict.Add("comma",Error "expected 'literal', but given 'comma'")
            dict.Add("f_cap",Error "expected 'literal', but given 'flag'")
            dict.Add("group",Error "expected 'literal', but given 'definition'")
            dict.Add("identifier",Error "expected 'literal', but given 'identifier'")
            dict.Add("lbrace",Error "expected 'literal', but given 'parentheses'")
            dict.Add("mid",Error "expected 'literal', but given 'mid'")
            dict.Add("num",Error "expected 'literal', but given 'literal'")
            dict.Add("prec",Error "expected 'literal', but given 'definition'")
            dict.Add("prod",Error "expected 'literal', but given 'definition'")
            dict.Add("rarrow",Error "expected 'literal', but given 'rarrow'")
            dict.Add("rbrace",Error "expected 'literal', but given 'parentheses'")
            dict.Add("string",Shift 9)
            dict.Add("token",Error "expected 'literal', but given 'token'")
            dict.Add("$",Error "expected 'literal', but given '$'")
            dict
            )(new Dictionary<string,Action>())
        //s29
        (fun (dict : Dictionary<string,Action>) ->
            dict.Add("as",Error "expected 'literal', but given 'as'")
            dict.Add("assoc",Error "expected 'literal', but given 'definition'")
            dict.Add("bang",Error "expected 'literal', but given 'definition'")
            dict.Add("colon",Error "expected 'literal', but given 'colon'")
            dict.Add("comma",Error "expected 'literal', but given 'comma'")
            dict.Add("f_cap",Error "expected 'literal', but given 'flag'")
            dict.Add("group",Error "expected 'literal', but given 'definition'")
            dict.Add("identifier",Error "expected 'literal', but given 'identifier'")
            dict.Add("lbrace",Error "expected 'literal', but given 'parentheses'")
            dict.Add("mid",Error "expected 'literal', but given 'mid'")
            dict.Add("num",Error "expected 'literal', but given 'literal'")
            dict.Add("prec",Error "expected 'literal', but given 'definition'")
            dict.Add("prod",Error "expected 'literal', but given 'definition'")
            dict.Add("rarrow",Error "expected 'literal', but given 'rarrow'")
            dict.Add("rbrace",Error "expected 'literal', but given 'parentheses'")
            dict.Add("string",Shift 9)
            dict.Add("token",Error "expected 'literal', but given 'token'")
            dict.Add("$",Error "expected 'literal', but given '$'")
            dict
            )(new Dictionary<string,Action>())
        //s30
        (fun (dict : Dictionary<string,Action>) ->
            dict.Add("as",Error "expected 'eoi', but given 'as'")
            dict.Add("assoc",Error "expected 'eoi', but given 'definition'")
            dict.Add("bang",Error "expected 'eoi', but given 'definition'")
            dict.Add("colon",Error "expected 'eoi', but given 'colon'")
            dict.Add("comma",Error "expected 'eoi', but given 'comma'")
            dict.Add("f_cap",Error "expected 'eoi', but given 'flag'")
            dict.Add("group",Error "expected 'eoi', but given 'definition'")
            dict.Add("identifier",Error "expected 'eoi', but given 'identifier'")
            dict.Add("lbrace",Error "expected 'eoi', but given 'parentheses'")
            dict.Add("mid",Error "expected 'eoi', but given 'mid'")
            dict.Add("num",Error "expected 'eoi', but given 'literal'")
            dict.Add("prec",Error "expected 'eoi', but given 'definition'")
            dict.Add("prod",Error "expected 'eoi', but given 'definition'")
            dict.Add("rarrow",Error "expected 'eoi', but given 'rarrow'")
            dict.Add("rbrace",Error "expected 'eoi', but given 'parentheses'")
            dict.Add("string",Error "expected 'eoi', but given 'literal'")
            dict.Add("token",Error "expected 'eoi', but given 'token'")
            dict.Add("$",Reduce 4)
            dict
            )(new Dictionary<string,Action>())
        //s31
        (fun (dict : Dictionary<string,Action>) ->
            dict.Add("as",Shift 42)
            dict.Add("assoc",Error "expected 'as', but given 'definition'")
            dict.Add("bang",Error "expected 'as', but given 'definition'")
            dict.Add("colon",Error "expected 'as', but given 'colon'")
            dict.Add("comma",Error "expected 'as', but given 'comma'")
            dict.Add("f_cap",Error "expected 'as', but given 'flag'")
            dict.Add("group",Error "expected 'as', but given 'definition'")
            dict.Add("identifier",Error "expected 'as', but given 'identifier'")
            dict.Add("lbrace",Error "expected 'as', but given 'parentheses'")
            dict.Add("mid",Error "expected 'as', but given 'mid'")
            dict.Add("num",Error "expected 'as', but given 'literal'")
            dict.Add("prec",Error "expected 'as', but given 'definition'")
            dict.Add("prod",Error "expected 'as', but given 'definition'")
            dict.Add("rarrow",Error "expected 'as', but given 'rarrow'")
            dict.Add("rbrace",Error "expected 'as', but given 'parentheses'")
            dict.Add("string",Error "expected 'as', but given 'literal'")
            dict.Add("token",Error "expected 'as', but given 'token'")
            dict.Add("$",Error "expected 'as', but given '$'")
            dict
            )(new Dictionary<string,Action>())
        //s32
        (fun (dict : Dictionary<string,Action>) ->
            dict.Add("as",Error "expected 'definition','token','eoi', but given 'as'")
            dict.Add("assoc",Shift 2)
            dict.Add("bang",Shift 3)
            dict.Add("colon",Error "expected 'definition','token','eoi', but given 'colon'")
            dict.Add("comma",Error "expected 'definition','token','eoi', but given 'comma'")
            dict.Add("f_cap",Error "expected 'definition','token','eoi', but given 'flag'")
            dict.Add("group",Shift 4)
            dict.Add("identifier",Error "expected 'definition','token','eoi', but given 'identifier'")
            dict.Add("lbrace",Error "expected 'definition','token','eoi', but given 'parentheses'")
            dict.Add("mid",Error "expected 'definition','token','eoi', but given 'mid'")
            dict.Add("num",Error "expected 'definition','token','eoi', but given 'literal'")
            dict.Add("prec",Shift 5)
            dict.Add("prod",Shift 6)
            dict.Add("rarrow",Error "expected 'definition','token','eoi', but given 'rarrow'")
            dict.Add("rbrace",Error "expected 'definition','token','eoi', but given 'parentheses'")
            dict.Add("string",Error "expected 'definition','token','eoi', but given 'literal'")
            dict.Add("token",Shift 7)
            dict.Add("$",Reduce 7)
            dict
            )(new Dictionary<string,Action>())
        //s33
        (fun (dict : Dictionary<string,Action>) ->
            dict.Add("as",Error "expected 'eoi', but given 'as'")
            dict.Add("assoc",Error "expected 'eoi', but given 'definition'")
            dict.Add("bang",Error "expected 'eoi', but given 'definition'")
            dict.Add("colon",Error "expected 'eoi', but given 'colon'")
            dict.Add("comma",Error "expected 'eoi', but given 'comma'")
            dict.Add("f_cap",Error "expected 'eoi', but given 'flag'")
            dict.Add("group",Error "expected 'eoi', but given 'definition'")
            dict.Add("identifier",Error "expected 'eoi', but given 'identifier'")
            dict.Add("lbrace",Error "expected 'eoi', but given 'parentheses'")
            dict.Add("mid",Error "expected 'eoi', but given 'mid'")
            dict.Add("num",Error "expected 'eoi', but given 'literal'")
            dict.Add("prec",Error "expected 'eoi', but given 'definition'")
            dict.Add("prod",Error "expected 'eoi', but given 'definition'")
            dict.Add("rarrow",Error "expected 'eoi', but given 'rarrow'")
            dict.Add("rbrace",Error "expected 'eoi', but given 'parentheses'")
            dict.Add("string",Error "expected 'eoi', but given 'literal'")
            dict.Add("token",Error "expected 'eoi', but given 'token'")
            dict.Add("$",Reduce 5)
            dict
            )(new Dictionary<string,Action>())
        //s34
        (fun (dict : Dictionary<string,Action>) ->
            dict.Add("as",Error "expected 'parentheses', but given 'as'")
            dict.Add("assoc",Error "expected 'parentheses', but given 'definition'")
            dict.Add("bang",Error "expected 'parentheses', but given 'definition'")
            dict.Add("colon",Error "expected 'parentheses', but given 'colon'")
            dict.Add("comma",Error "expected 'parentheses', but given 'comma'")
            dict.Add("f_cap",Error "expected 'parentheses', but given 'flag'")
            dict.Add("group",Error "expected 'parentheses', but given 'definition'")
            dict.Add("identifier",Error "expected 'parentheses', but given 'identifier'")
            dict.Add("lbrace",Error "expected 'parentheses', but given 'parentheses'")
            dict.Add("mid",Error "expected 'parentheses', but given 'mid'")
            dict.Add("num",Error "expected 'parentheses', but given 'literal'")
            dict.Add("prec",Error "expected 'parentheses', but given 'definition'")
            dict.Add("prod",Error "expected 'parentheses', but given 'definition'")
            dict.Add("rarrow",Error "expected 'parentheses', but given 'rarrow'")
            dict.Add("rbrace",Shift 44)
            dict.Add("string",Error "expected 'parentheses', but given 'literal'")
            dict.Add("token",Error "expected 'parentheses', but given 'token'")
            dict.Add("$",Error "expected 'parentheses', but given '$'")
            dict
            )(new Dictionary<string,Action>())
        //s35
        (fun (dict : Dictionary<string,Action>) ->
            dict.Add("as",Error "expected 'eoi', but given 'as'")
            dict.Add("assoc",Error "expected 'eoi', but given 'definition'")
            dict.Add("bang",Error "expected 'eoi', but given 'definition'")
            dict.Add("colon",Error "expected 'eoi', but given 'colon'")
            dict.Add("comma",Error "expected 'eoi', but given 'comma'")
            dict.Add("f_cap",Error "expected 'eoi', but given 'flag'")
            dict.Add("group",Error "expected 'eoi', but given 'definition'")
            dict.Add("identifier",Error "expected 'eoi', but given 'identifier'")
            dict.Add("lbrace",Error "expected 'eoi', but given 'parentheses'")
            dict.Add("mid",Error "expected 'eoi', but given 'mid'")
            dict.Add("num",Error "expected 'eoi', but given 'literal'")
            dict.Add("prec",Error "expected 'eoi', but given 'definition'")
            dict.Add("prod",Error "expected 'eoi', but given 'definition'")
            dict.Add("rarrow",Error "expected 'eoi', but given 'rarrow'")
            dict.Add("rbrace",Error "expected 'eoi', but given 'parentheses'")
            dict.Add("string",Error "expected 'eoi', but given 'literal'")
            dict.Add("token",Error "expected 'eoi', but given 'token'")
            dict.Add("$",Reduce 2)
            dict
            )(new Dictionary<string,Action>())
        //s36
        (fun (dict : Dictionary<string,Action>) ->
            dict.Add("as",Error "expected 'definition','identifier','mid','literal','token','eoi', but given 'as'")
            dict.Add("assoc",Reduce 11)
            dict.Add("bang",Reduce 11)
            dict.Add("colon",Error "expected 'definition','identifier','mid','literal','token','eoi', but given 'colon'")
            dict.Add("comma",Error "expected 'definition','identifier','mid','literal','token','eoi', but given 'comma'")
            dict.Add("f_cap",Error "expected 'definition','identifier','mid','literal','token','eoi', but given 'flag'")
            dict.Add("group",Reduce 11)
            dict.Add("identifier",Shift 15)
            dict.Add("lbrace",Error "expected 'definition','identifier','mid','literal','token','eoi', but given 'parentheses'")
            dict.Add("mid",Shift 39)
            dict.Add("num",Error "expected 'definition','identifier','mid','literal','token','eoi', but given 'literal'")
            dict.Add("prec",Reduce 11)
            dict.Add("prod",Reduce 11)
            dict.Add("rarrow",Error "expected 'definition','identifier','mid','literal','token','eoi', but given 'rarrow'")
            dict.Add("rbrace",Error "expected 'definition','identifier','mid','literal','token','eoi', but given 'parentheses'")
            dict.Add("string",Shift 9)
            dict.Add("token",Reduce 11)
            dict.Add("$",Reduce 11)
            dict
            )(new Dictionary<string,Action>())
        //s37
        (fun (dict : Dictionary<string,Action>) ->
            dict.Add("as",Error "expected 'definition','token','eoi', but given 'as'")
            dict.Add("assoc",Shift 2)
            dict.Add("bang",Shift 3)
            dict.Add("colon",Error "expected 'definition','token','eoi', but given 'colon'")
            dict.Add("comma",Error "expected 'definition','token','eoi', but given 'comma'")
            dict.Add("f_cap",Error "expected 'definition','token','eoi', but given 'flag'")
            dict.Add("group",Shift 4)
            dict.Add("identifier",Error "expected 'definition','token','eoi', but given 'identifier'")
            dict.Add("lbrace",Error "expected 'definition','token','eoi', but given 'parentheses'")
            dict.Add("mid",Error "expected 'definition','token','eoi', but given 'mid'")
            dict.Add("num",Error "expected 'definition','token','eoi', but given 'literal'")
            dict.Add("prec",Shift 5)
            dict.Add("prod",Shift 6)
            dict.Add("rarrow",Error "expected 'definition','token','eoi', but given 'rarrow'")
            dict.Add("rbrace",Error "expected 'definition','token','eoi', but given 'parentheses'")
            dict.Add("string",Error "expected 'definition','token','eoi', but given 'literal'")
            dict.Add("token",Shift 7)
            dict.Add("$",Reduce 7)
            dict
            )(new Dictionary<string,Action>())
        //s38
        (fun (dict : Dictionary<string,Action>) ->
            dict.Add("as",Error "expected 'definition','identifier','mid','literal','token','eoi', but given 'as'")
            dict.Add("assoc",Reduce 11)
            dict.Add("bang",Reduce 11)
            dict.Add("colon",Error "expected 'definition','identifier','mid','literal','token','eoi', but given 'colon'")
            dict.Add("comma",Error "expected 'definition','identifier','mid','literal','token','eoi', but given 'comma'")
            dict.Add("f_cap",Error "expected 'definition','identifier','mid','literal','token','eoi', but given 'flag'")
            dict.Add("group",Reduce 11)
            dict.Add("identifier",Shift 15)
            dict.Add("lbrace",Error "expected 'definition','identifier','mid','literal','token','eoi', but given 'parentheses'")
            dict.Add("mid",Shift 39)
            dict.Add("num",Error "expected 'definition','identifier','mid','literal','token','eoi', but given 'literal'")
            dict.Add("prec",Reduce 11)
            dict.Add("prod",Reduce 11)
            dict.Add("rarrow",Error "expected 'definition','identifier','mid','literal','token','eoi', but given 'rarrow'")
            dict.Add("rbrace",Error "expected 'definition','identifier','mid','literal','token','eoi', but given 'parentheses'")
            dict.Add("string",Shift 9)
            dict.Add("token",Reduce 11)
            dict.Add("$",Reduce 11)
            dict
            )(new Dictionary<string,Action>())
        //s39
        (fun (dict : Dictionary<string,Action>) ->
            dict.Add("as",Error "expected 'definition','identifier','mid','literal','token','eoi', but given 'as'")
            dict.Add("assoc",Reduce 11)
            dict.Add("bang",Reduce 11)
            dict.Add("colon",Error "expected 'definition','identifier','mid','literal','token','eoi', but given 'colon'")
            dict.Add("comma",Error "expected 'definition','identifier','mid','literal','token','eoi', but given 'comma'")
            dict.Add("f_cap",Error "expected 'definition','identifier','mid','literal','token','eoi', but given 'flag'")
            dict.Add("group",Reduce 11)
            dict.Add("identifier",Shift 15)
            dict.Add("lbrace",Error "expected 'definition','identifier','mid','literal','token','eoi', but given 'parentheses'")
            dict.Add("mid",Shift 39)
            dict.Add("num",Error "expected 'definition','identifier','mid','literal','token','eoi', but given 'literal'")
            dict.Add("prec",Reduce 11)
            dict.Add("prod",Reduce 11)
            dict.Add("rarrow",Error "expected 'definition','identifier','mid','literal','token','eoi', but given 'rarrow'")
            dict.Add("rbrace",Error "expected 'definition','identifier','mid','literal','token','eoi', but given 'parentheses'")
            dict.Add("string",Shift 9)
            dict.Add("token",Reduce 11)
            dict.Add("$",Reduce 11)
            dict
            )(new Dictionary<string,Action>())
        //s40
        (fun (dict : Dictionary<string,Action>) ->
            dict.Add("as",Reduce 17)
            dict.Add("assoc",Reduce 17)
            dict.Add("bang",Reduce 17)
            dict.Add("colon",Error "expected 'as','definition','parentheses','token','eoi', but given 'colon'")
            dict.Add("comma",Error "expected 'as','definition','parentheses','token','eoi', but given 'comma'")
            dict.Add("f_cap",Error "expected 'as','definition','parentheses','token','eoi', but given 'flag'")
            dict.Add("group",Reduce 17)
            dict.Add("identifier",Error "expected 'as','definition','parentheses','token','eoi', but given 'identifier'")
            dict.Add("lbrace",Error "expected 'as','definition','parentheses','token','eoi', but given 'parentheses'")
            dict.Add("mid",Error "expected 'as','definition','parentheses','token','eoi', but given 'mid'")
            dict.Add("num",Error "expected 'as','definition','parentheses','token','eoi', but given 'literal'")
            dict.Add("prec",Reduce 17)
            dict.Add("prod",Reduce 17)
            dict.Add("rarrow",Error "expected 'as','definition','parentheses','token','eoi', but given 'rarrow'")
            dict.Add("rbrace",Reduce 17)
            dict.Add("string",Error "expected 'as','definition','parentheses','token','eoi', but given 'literal'")
            dict.Add("token",Reduce 17)
            dict.Add("$",Reduce 17)
            dict
            )(new Dictionary<string,Action>())
        //s41
        (fun (dict : Dictionary<string,Action>) ->
            dict.Add("as",Error "expected 'definition','token','eoi', but given 'as'")
            dict.Add("assoc",Reduce 12)
            dict.Add("bang",Reduce 12)
            dict.Add("colon",Error "expected 'definition','token','eoi', but given 'colon'")
            dict.Add("comma",Error "expected 'definition','token','eoi', but given 'comma'")
            dict.Add("f_cap",Error "expected 'definition','token','eoi', but given 'flag'")
            dict.Add("group",Reduce 12)
            dict.Add("identifier",Error "expected 'definition','token','eoi', but given 'identifier'")
            dict.Add("lbrace",Error "expected 'definition','token','eoi', but given 'parentheses'")
            dict.Add("mid",Error "expected 'definition','token','eoi', but given 'mid'")
            dict.Add("num",Error "expected 'definition','token','eoi', but given 'literal'")
            dict.Add("prec",Reduce 12)
            dict.Add("prod",Reduce 12)
            dict.Add("rarrow",Error "expected 'definition','token','eoi', but given 'rarrow'")
            dict.Add("rbrace",Error "expected 'definition','token','eoi', but given 'parentheses'")
            dict.Add("string",Error "expected 'definition','token','eoi', but given 'literal'")
            dict.Add("token",Reduce 12)
            dict.Add("$",Reduce 12)
            dict
            )(new Dictionary<string,Action>())
        //s42
        (fun (dict : Dictionary<string,Action>) ->
            dict.Add("as",Error "expected 'literal', but given 'as'")
            dict.Add("assoc",Error "expected 'literal', but given 'definition'")
            dict.Add("bang",Error "expected 'literal', but given 'definition'")
            dict.Add("colon",Error "expected 'literal', but given 'colon'")
            dict.Add("comma",Error "expected 'literal', but given 'comma'")
            dict.Add("f_cap",Error "expected 'literal', but given 'flag'")
            dict.Add("group",Error "expected 'literal', but given 'definition'")
            dict.Add("identifier",Error "expected 'literal', but given 'identifier'")
            dict.Add("lbrace",Error "expected 'literal', but given 'parentheses'")
            dict.Add("mid",Error "expected 'literal', but given 'mid'")
            dict.Add("num",Error "expected 'literal', but given 'literal'")
            dict.Add("prec",Error "expected 'literal', but given 'definition'")
            dict.Add("prod",Error "expected 'literal', but given 'definition'")
            dict.Add("rarrow",Error "expected 'literal', but given 'rarrow'")
            dict.Add("rbrace",Error "expected 'literal', but given 'parentheses'")
            dict.Add("string",Shift 9)
            dict.Add("token",Error "expected 'literal', but given 'token'")
            dict.Add("$",Error "expected 'literal', but given '$'")
            dict
            )(new Dictionary<string,Action>())
        //s43
        (fun (dict : Dictionary<string,Action>) ->
            dict.Add("as",Error "expected 'eoi', but given 'as'")
            dict.Add("assoc",Error "expected 'eoi', but given 'definition'")
            dict.Add("bang",Error "expected 'eoi', but given 'definition'")
            dict.Add("colon",Error "expected 'eoi', but given 'colon'")
            dict.Add("comma",Error "expected 'eoi', but given 'comma'")
            dict.Add("f_cap",Error "expected 'eoi', but given 'flag'")
            dict.Add("group",Error "expected 'eoi', but given 'definition'")
            dict.Add("identifier",Error "expected 'eoi', but given 'identifier'")
            dict.Add("lbrace",Error "expected 'eoi', but given 'parentheses'")
            dict.Add("mid",Error "expected 'eoi', but given 'mid'")
            dict.Add("num",Error "expected 'eoi', but given 'literal'")
            dict.Add("prec",Error "expected 'eoi', but given 'definition'")
            dict.Add("prod",Error "expected 'eoi', but given 'definition'")
            dict.Add("rarrow",Error "expected 'eoi', but given 'rarrow'")
            dict.Add("rbrace",Error "expected 'eoi', but given 'parentheses'")
            dict.Add("string",Error "expected 'eoi', but given 'literal'")
            dict.Add("token",Error "expected 'eoi', but given 'token'")
            dict.Add("$",Reduce 3)
            dict
            )(new Dictionary<string,Action>())
        //s44
        (fun (dict : Dictionary<string,Action>) ->
            dict.Add("as",Error "expected 'definition','token','eoi', but given 'as'")
            dict.Add("assoc",Reduce 15)
            dict.Add("bang",Reduce 15)
            dict.Add("colon",Error "expected 'definition','token','eoi', but given 'colon'")
            dict.Add("comma",Error "expected 'definition','token','eoi', but given 'comma'")
            dict.Add("f_cap",Error "expected 'definition','token','eoi', but given 'flag'")
            dict.Add("group",Reduce 15)
            dict.Add("identifier",Error "expected 'definition','token','eoi', but given 'identifier'")
            dict.Add("lbrace",Error "expected 'definition','token','eoi', but given 'parentheses'")
            dict.Add("mid",Error "expected 'definition','token','eoi', but given 'mid'")
            dict.Add("num",Error "expected 'definition','token','eoi', but given 'literal'")
            dict.Add("prec",Reduce 15)
            dict.Add("prod",Reduce 15)
            dict.Add("rarrow",Error "expected 'definition','token','eoi', but given 'rarrow'")
            dict.Add("rbrace",Error "expected 'definition','token','eoi', but given 'parentheses'")
            dict.Add("string",Error "expected 'definition','token','eoi', but given 'literal'")
            dict.Add("token",Reduce 15)
            dict.Add("$",Reduce 15)
            dict
            )(new Dictionary<string,Action>())
        //s45
        (fun (dict : Dictionary<string,Action>) ->
            dict.Add("as",Error "expected 'definition','token','eoi', but given 'as'")
            dict.Add("assoc",Reduce 9)
            dict.Add("bang",Reduce 9)
            dict.Add("colon",Error "expected 'definition','token','eoi', but given 'colon'")
            dict.Add("comma",Error "expected 'definition','token','eoi', but given 'comma'")
            dict.Add("f_cap",Error "expected 'definition','token','eoi', but given 'flag'")
            dict.Add("group",Reduce 9)
            dict.Add("identifier",Error "expected 'definition','token','eoi', but given 'identifier'")
            dict.Add("lbrace",Error "expected 'definition','token','eoi', but given 'parentheses'")
            dict.Add("mid",Error "expected 'definition','token','eoi', but given 'mid'")
            dict.Add("num",Error "expected 'definition','token','eoi', but given 'literal'")
            dict.Add("prec",Reduce 9)
            dict.Add("prod",Reduce 9)
            dict.Add("rarrow",Error "expected 'definition','token','eoi', but given 'rarrow'")
            dict.Add("rbrace",Error "expected 'definition','token','eoi', but given 'parentheses'")
            dict.Add("string",Error "expected 'definition','token','eoi', but given 'literal'")
            dict.Add("token",Reduce 9)
            dict.Add("$",Reduce 9)
            dict
            )(new Dictionary<string,Action>())
        //s46
        (fun (dict : Dictionary<string,Action>) ->
            dict.Add("as",Error "expected 'eoi', but given 'as'")
            dict.Add("assoc",Error "expected 'eoi', but given 'definition'")
            dict.Add("bang",Error "expected 'eoi', but given 'definition'")
            dict.Add("colon",Error "expected 'eoi', but given 'colon'")
            dict.Add("comma",Error "expected 'eoi', but given 'comma'")
            dict.Add("f_cap",Error "expected 'eoi', but given 'flag'")
            dict.Add("group",Error "expected 'eoi', but given 'definition'")
            dict.Add("identifier",Error "expected 'eoi', but given 'identifier'")
            dict.Add("lbrace",Error "expected 'eoi', but given 'parentheses'")
            dict.Add("mid",Error "expected 'eoi', but given 'mid'")
            dict.Add("num",Error "expected 'eoi', but given 'literal'")
            dict.Add("prec",Error "expected 'eoi', but given 'definition'")
            dict.Add("prod",Error "expected 'eoi', but given 'definition'")
            dict.Add("rarrow",Error "expected 'eoi', but given 'rarrow'")
            dict.Add("rbrace",Error "expected 'eoi', but given 'parentheses'")
            dict.Add("string",Error "expected 'eoi', but given 'literal'")
            dict.Add("token",Error "expected 'eoi', but given 'token'")
            dict.Add("$",Reduce 1)
            dict
            )(new Dictionary<string,Action>())
        //s47
        (fun (dict : Dictionary<string,Action>) ->
            dict.Add("as",Error "expected 'definition','token','eoi', but given 'as'")
            dict.Add("assoc",Reduce 8)
            dict.Add("bang",Reduce 8)
            dict.Add("colon",Error "expected 'definition','token','eoi', but given 'colon'")
            dict.Add("comma",Error "expected 'definition','token','eoi', but given 'comma'")
            dict.Add("f_cap",Error "expected 'definition','token','eoi', but given 'flag'")
            dict.Add("group",Reduce 8)
            dict.Add("identifier",Error "expected 'definition','token','eoi', but given 'identifier'")
            dict.Add("lbrace",Error "expected 'definition','token','eoi', but given 'parentheses'")
            dict.Add("mid",Error "expected 'definition','token','eoi', but given 'mid'")
            dict.Add("num",Error "expected 'definition','token','eoi', but given 'literal'")
            dict.Add("prec",Reduce 8)
            dict.Add("prod",Reduce 8)
            dict.Add("rarrow",Error "expected 'definition','token','eoi', but given 'rarrow'")
            dict.Add("rbrace",Error "expected 'definition','token','eoi', but given 'parentheses'")
            dict.Add("string",Error "expected 'definition','token','eoi', but given 'literal'")
            dict.Add("token",Reduce 8)
            dict.Add("$",Reduce 8)
            dict
            )(new Dictionary<string,Action>())
        //s48
        (fun (dict : Dictionary<string,Action>) ->
            dict.Add("as",Error "expected 'definition','token','eoi', but given 'as'")
            dict.Add("assoc",Reduce 10)
            dict.Add("bang",Reduce 10)
            dict.Add("colon",Error "expected 'definition','token','eoi', but given 'colon'")
            dict.Add("comma",Error "expected 'definition','token','eoi', but given 'comma'")
            dict.Add("f_cap",Error "expected 'definition','token','eoi', but given 'flag'")
            dict.Add("group",Reduce 10)
            dict.Add("identifier",Error "expected 'definition','token','eoi', but given 'identifier'")
            dict.Add("lbrace",Error "expected 'definition','token','eoi', but given 'parentheses'")
            dict.Add("mid",Error "expected 'definition','token','eoi', but given 'mid'")
            dict.Add("num",Error "expected 'definition','token','eoi', but given 'literal'")
            dict.Add("prec",Reduce 10)
            dict.Add("prod",Reduce 10)
            dict.Add("rarrow",Error "expected 'definition','token','eoi', but given 'rarrow'")
            dict.Add("rbrace",Error "expected 'definition','token','eoi', but given 'parentheses'")
            dict.Add("string",Error "expected 'definition','token','eoi', but given 'literal'")
            dict.Add("token",Reduce 10)
            dict.Add("$",Reduce 10)
            dict
            )(new Dictionary<string,Action>())
        //s49
        (fun (dict : Dictionary<string,Action>) ->
            dict.Add("as",Error "expected 'definition','token','eoi', but given 'as'")
            dict.Add("assoc",Reduce 13)
            dict.Add("bang",Reduce 13)
            dict.Add("colon",Error "expected 'definition','token','eoi', but given 'colon'")
            dict.Add("comma",Error "expected 'definition','token','eoi', but given 'comma'")
            dict.Add("f_cap",Error "expected 'definition','token','eoi', but given 'flag'")
            dict.Add("group",Reduce 13)
            dict.Add("identifier",Error "expected 'definition','token','eoi', but given 'identifier'")
            dict.Add("lbrace",Error "expected 'definition','token','eoi', but given 'parentheses'")
            dict.Add("mid",Error "expected 'definition','token','eoi', but given 'mid'")
            dict.Add("num",Error "expected 'definition','token','eoi', but given 'literal'")
            dict.Add("prec",Reduce 13)
            dict.Add("prod",Reduce 13)
            dict.Add("rarrow",Error "expected 'definition','token','eoi', but given 'rarrow'")
            dict.Add("rbrace",Error "expected 'definition','token','eoi', but given 'parentheses'")
            dict.Add("string",Error "expected 'definition','token','eoi', but given 'literal'")
            dict.Add("token",Reduce 13)
            dict.Add("$",Reduce 13)
            dict
            )(new Dictionary<string,Action>())
        |]
    let gotoTable = [|
        //s0
        (fun (dict : Dictionary<string,Option<int>>) ->
            dict.Add("BangToken",None)
            dict.Add("Exp",Some 1)
            dict.Add("Group",None)
            dict.Add("Identifier",None)
            dict.Add("Num",None)
            dict.Add("ProdRight",None)
            dict.Add("String",None)
            dict.Add("StringTuple",None)
            dict.Add("Token",None)
            dict
            )(new Dictionary<string,Option<int>>())
        //s1
        (fun (dict : Dictionary<string,Option<int>>) ->
            dict.Add("BangToken",None)
            dict.Add("Exp",None)
            dict.Add("Group",None)
            dict.Add("Identifier",None)
            dict.Add("Num",None)
            dict.Add("ProdRight",None)
            dict.Add("String",None)
            dict.Add("StringTuple",None)
            dict.Add("Token",None)
            dict
            )(new Dictionary<string,Option<int>>())
        //s2
        (fun (dict : Dictionary<string,Option<int>>) ->
            dict.Add("BangToken",None)
            dict.Add("Exp",None)
            dict.Add("Group",None)
            dict.Add("Identifier",None)
            dict.Add("Num",None)
            dict.Add("ProdRight",None)
            dict.Add("String",Some 8)
            dict.Add("StringTuple",None)
            dict.Add("Token",None)
            dict
            )(new Dictionary<string,Option<int>>())
        //s3
        (fun (dict : Dictionary<string,Option<int>>) ->
            dict.Add("BangToken",None)
            dict.Add("Exp",None)
            dict.Add("Group",None)
            dict.Add("Identifier",None)
            dict.Add("Num",None)
            dict.Add("ProdRight",None)
            dict.Add("String",None)
            dict.Add("StringTuple",None)
            dict.Add("Token",None)
            dict
            )(new Dictionary<string,Option<int>>())
        //s4
        (fun (dict : Dictionary<string,Option<int>>) ->
            dict.Add("BangToken",None)
            dict.Add("Exp",None)
            dict.Add("Group",Some 11)
            dict.Add("Identifier",None)
            dict.Add("Num",None)
            dict.Add("ProdRight",None)
            dict.Add("String",Some 12)
            dict.Add("StringTuple",None)
            dict.Add("Token",None)
            dict
            )(new Dictionary<string,Option<int>>())
        //s5
        (fun (dict : Dictionary<string,Option<int>>) ->
            dict.Add("BangToken",None)
            dict.Add("Exp",None)
            dict.Add("Group",None)
            dict.Add("Identifier",None)
            dict.Add("Num",None)
            dict.Add("ProdRight",None)
            dict.Add("String",Some 13)
            dict.Add("StringTuple",None)
            dict.Add("Token",None)
            dict
            )(new Dictionary<string,Option<int>>())
        //s6
        (fun (dict : Dictionary<string,Option<int>>) ->
            dict.Add("BangToken",None)
            dict.Add("Exp",None)
            dict.Add("Group",None)
            dict.Add("Identifier",Some 14)
            dict.Add("Num",None)
            dict.Add("ProdRight",None)
            dict.Add("String",None)
            dict.Add("StringTuple",None)
            dict.Add("Token",None)
            dict
            )(new Dictionary<string,Option<int>>())
        //s7
        (fun (dict : Dictionary<string,Option<int>>) ->
            dict.Add("BangToken",None)
            dict.Add("Exp",None)
            dict.Add("Group",None)
            dict.Add("Identifier",None)
            dict.Add("Num",None)
            dict.Add("ProdRight",None)
            dict.Add("String",Some 16)
            dict.Add("StringTuple",Some 17)
            dict.Add("Token",Some 18)
            dict
            )(new Dictionary<string,Option<int>>())
        //s8
        (fun (dict : Dictionary<string,Option<int>>) ->
            dict.Add("BangToken",None)
            dict.Add("Exp",None)
            dict.Add("Group",None)
            dict.Add("Identifier",None)
            dict.Add("Num",None)
            dict.Add("ProdRight",None)
            dict.Add("String",None)
            dict.Add("StringTuple",None)
            dict.Add("Token",None)
            dict
            )(new Dictionary<string,Option<int>>())
        //s9
        (fun (dict : Dictionary<string,Option<int>>) ->
            dict.Add("BangToken",None)
            dict.Add("Exp",None)
            dict.Add("Group",None)
            dict.Add("Identifier",None)
            dict.Add("Num",None)
            dict.Add("ProdRight",None)
            dict.Add("String",None)
            dict.Add("StringTuple",None)
            dict.Add("Token",None)
            dict
            )(new Dictionary<string,Option<int>>())
        //s10
        (fun (dict : Dictionary<string,Option<int>>) ->
            dict.Add("BangToken",Some 21)
            dict.Add("Exp",None)
            dict.Add("Group",None)
            dict.Add("Identifier",None)
            dict.Add("Num",None)
            dict.Add("ProdRight",None)
            dict.Add("String",Some 16)
            dict.Add("StringTuple",Some 22)
            dict.Add("Token",None)
            dict
            )(new Dictionary<string,Option<int>>())
        //s11
        (fun (dict : Dictionary<string,Option<int>>) ->
            dict.Add("BangToken",None)
            dict.Add("Exp",Some 23)
            dict.Add("Group",None)
            dict.Add("Identifier",None)
            dict.Add("Num",None)
            dict.Add("ProdRight",None)
            dict.Add("String",None)
            dict.Add("StringTuple",None)
            dict.Add("Token",None)
            dict
            )(new Dictionary<string,Option<int>>())
        //s12
        (fun (dict : Dictionary<string,Option<int>>) ->
            dict.Add("BangToken",None)
            dict.Add("Exp",None)
            dict.Add("Group",None)
            dict.Add("Identifier",None)
            dict.Add("Num",None)
            dict.Add("ProdRight",None)
            dict.Add("String",None)
            dict.Add("StringTuple",None)
            dict.Add("Token",None)
            dict
            )(new Dictionary<string,Option<int>>())
        //s13
        (fun (dict : Dictionary<string,Option<int>>) ->
            dict.Add("BangToken",None)
            dict.Add("Exp",None)
            dict.Add("Group",None)
            dict.Add("Identifier",None)
            dict.Add("Num",Some 25)
            dict.Add("ProdRight",None)
            dict.Add("String",None)
            dict.Add("StringTuple",None)
            dict.Add("Token",None)
            dict
            )(new Dictionary<string,Option<int>>())
        //s14
        (fun (dict : Dictionary<string,Option<int>>) ->
            dict.Add("BangToken",None)
            dict.Add("Exp",None)
            dict.Add("Group",None)
            dict.Add("Identifier",None)
            dict.Add("Num",None)
            dict.Add("ProdRight",None)
            dict.Add("String",None)
            dict.Add("StringTuple",None)
            dict.Add("Token",None)
            dict
            )(new Dictionary<string,Option<int>>())
        //s15
        (fun (dict : Dictionary<string,Option<int>>) ->
            dict.Add("BangToken",None)
            dict.Add("Exp",None)
            dict.Add("Group",None)
            dict.Add("Identifier",None)
            dict.Add("Num",None)
            dict.Add("ProdRight",None)
            dict.Add("String",None)
            dict.Add("StringTuple",None)
            dict.Add("Token",None)
            dict
            )(new Dictionary<string,Option<int>>())
        //s16
        (fun (dict : Dictionary<string,Option<int>>) ->
            dict.Add("BangToken",None)
            dict.Add("Exp",None)
            dict.Add("Group",None)
            dict.Add("Identifier",None)
            dict.Add("Num",None)
            dict.Add("ProdRight",None)
            dict.Add("String",None)
            dict.Add("StringTuple",None)
            dict.Add("Token",None)
            dict
            )(new Dictionary<string,Option<int>>())
        //s17
        (fun (dict : Dictionary<string,Option<int>>) ->
            dict.Add("BangToken",None)
            dict.Add("Exp",None)
            dict.Add("Group",None)
            dict.Add("Identifier",None)
            dict.Add("Num",None)
            dict.Add("ProdRight",None)
            dict.Add("String",None)
            dict.Add("StringTuple",None)
            dict.Add("Token",None)
            dict
            )(new Dictionary<string,Option<int>>())
        //s18
        (fun (dict : Dictionary<string,Option<int>>) ->
            dict.Add("BangToken",None)
            dict.Add("Exp",Some 30)
            dict.Add("Group",None)
            dict.Add("Identifier",None)
            dict.Add("Num",None)
            dict.Add("ProdRight",None)
            dict.Add("String",None)
            dict.Add("StringTuple",None)
            dict.Add("Token",None)
            dict
            )(new Dictionary<string,Option<int>>())
        //s19
        (fun (dict : Dictionary<string,Option<int>>) ->
            dict.Add("BangToken",None)
            dict.Add("Exp",None)
            dict.Add("Group",None)
            dict.Add("Identifier",None)
            dict.Add("Num",None)
            dict.Add("ProdRight",None)
            dict.Add("String",Some 16)
            dict.Add("StringTuple",Some 31)
            dict.Add("Token",None)
            dict
            )(new Dictionary<string,Option<int>>())
        //s20
        (fun (dict : Dictionary<string,Option<int>>) ->
            dict.Add("BangToken",None)
            dict.Add("Exp",None)
            dict.Add("Group",None)
            dict.Add("Identifier",None)
            dict.Add("Num",None)
            dict.Add("ProdRight",None)
            dict.Add("String",Some 16)
            dict.Add("StringTuple",Some 32)
            dict.Add("Token",None)
            dict
            )(new Dictionary<string,Option<int>>())
        //s21
        (fun (dict : Dictionary<string,Option<int>>) ->
            dict.Add("BangToken",None)
            dict.Add("Exp",Some 33)
            dict.Add("Group",None)
            dict.Add("Identifier",None)
            dict.Add("Num",None)
            dict.Add("ProdRight",None)
            dict.Add("String",None)
            dict.Add("StringTuple",None)
            dict.Add("Token",None)
            dict
            )(new Dictionary<string,Option<int>>())
        //s22
        (fun (dict : Dictionary<string,Option<int>>) ->
            dict.Add("BangToken",None)
            dict.Add("Exp",None)
            dict.Add("Group",None)
            dict.Add("Identifier",None)
            dict.Add("Num",None)
            dict.Add("ProdRight",None)
            dict.Add("String",None)
            dict.Add("StringTuple",None)
            dict.Add("Token",None)
            dict
            )(new Dictionary<string,Option<int>>())
        //s23
        (fun (dict : Dictionary<string,Option<int>>) ->
            dict.Add("BangToken",None)
            dict.Add("Exp",None)
            dict.Add("Group",None)
            dict.Add("Identifier",None)
            dict.Add("Num",None)
            dict.Add("ProdRight",None)
            dict.Add("String",None)
            dict.Add("StringTuple",None)
            dict.Add("Token",None)
            dict
            )(new Dictionary<string,Option<int>>())
        //s24
        (fun (dict : Dictionary<string,Option<int>>) ->
            dict.Add("BangToken",None)
            dict.Add("Exp",None)
            dict.Add("Group",None)
            dict.Add("Identifier",None)
            dict.Add("Num",None)
            dict.Add("ProdRight",None)
            dict.Add("String",Some 16)
            dict.Add("StringTuple",Some 34)
            dict.Add("Token",None)
            dict
            )(new Dictionary<string,Option<int>>())
        //s25
        (fun (dict : Dictionary<string,Option<int>>) ->
            dict.Add("BangToken",None)
            dict.Add("Exp",Some 35)
            dict.Add("Group",None)
            dict.Add("Identifier",None)
            dict.Add("Num",None)
            dict.Add("ProdRight",None)
            dict.Add("String",None)
            dict.Add("StringTuple",None)
            dict.Add("Token",None)
            dict
            )(new Dictionary<string,Option<int>>())
        //s26
        (fun (dict : Dictionary<string,Option<int>>) ->
            dict.Add("BangToken",None)
            dict.Add("Exp",None)
            dict.Add("Group",None)
            dict.Add("Identifier",None)
            dict.Add("Num",None)
            dict.Add("ProdRight",None)
            dict.Add("String",None)
            dict.Add("StringTuple",None)
            dict.Add("Token",None)
            dict
            )(new Dictionary<string,Option<int>>())
        //s27
        (fun (dict : Dictionary<string,Option<int>>) ->
            dict.Add("BangToken",None)
            dict.Add("Exp",None)
            dict.Add("Group",None)
            dict.Add("Identifier",Some 36)
            dict.Add("Num",None)
            dict.Add("ProdRight",Some 37)
            dict.Add("String",Some 38)
            dict.Add("StringTuple",None)
            dict.Add("Token",None)
            dict
            )(new Dictionary<string,Option<int>>())
        //s28
        (fun (dict : Dictionary<string,Option<int>>) ->
            dict.Add("BangToken",None)
            dict.Add("Exp",None)
            dict.Add("Group",None)
            dict.Add("Identifier",None)
            dict.Add("Num",None)
            dict.Add("ProdRight",None)
            dict.Add("String",Some 16)
            dict.Add("StringTuple",Some 40)
            dict.Add("Token",None)
            dict
            )(new Dictionary<string,Option<int>>())
        //s29
        (fun (dict : Dictionary<string,Option<int>>) ->
            dict.Add("BangToken",None)
            dict.Add("Exp",None)
            dict.Add("Group",None)
            dict.Add("Identifier",None)
            dict.Add("Num",None)
            dict.Add("ProdRight",None)
            dict.Add("String",Some 16)
            dict.Add("StringTuple",Some 41)
            dict.Add("Token",None)
            dict
            )(new Dictionary<string,Option<int>>())
        //s30
        (fun (dict : Dictionary<string,Option<int>>) ->
            dict.Add("BangToken",None)
            dict.Add("Exp",None)
            dict.Add("Group",None)
            dict.Add("Identifier",None)
            dict.Add("Num",None)
            dict.Add("ProdRight",None)
            dict.Add("String",None)
            dict.Add("StringTuple",None)
            dict.Add("Token",None)
            dict
            )(new Dictionary<string,Option<int>>())
        //s31
        (fun (dict : Dictionary<string,Option<int>>) ->
            dict.Add("BangToken",None)
            dict.Add("Exp",None)
            dict.Add("Group",None)
            dict.Add("Identifier",None)
            dict.Add("Num",None)
            dict.Add("ProdRight",None)
            dict.Add("String",None)
            dict.Add("StringTuple",None)
            dict.Add("Token",None)
            dict
            )(new Dictionary<string,Option<int>>())
        //s32
        (fun (dict : Dictionary<string,Option<int>>) ->
            dict.Add("BangToken",None)
            dict.Add("Exp",Some 43)
            dict.Add("Group",None)
            dict.Add("Identifier",None)
            dict.Add("Num",None)
            dict.Add("ProdRight",None)
            dict.Add("String",None)
            dict.Add("StringTuple",None)
            dict.Add("Token",None)
            dict
            )(new Dictionary<string,Option<int>>())
        //s33
        (fun (dict : Dictionary<string,Option<int>>) ->
            dict.Add("BangToken",None)
            dict.Add("Exp",None)
            dict.Add("Group",None)
            dict.Add("Identifier",None)
            dict.Add("Num",None)
            dict.Add("ProdRight",None)
            dict.Add("String",None)
            dict.Add("StringTuple",None)
            dict.Add("Token",None)
            dict
            )(new Dictionary<string,Option<int>>())
        //s34
        (fun (dict : Dictionary<string,Option<int>>) ->
            dict.Add("BangToken",None)
            dict.Add("Exp",None)
            dict.Add("Group",None)
            dict.Add("Identifier",None)
            dict.Add("Num",None)
            dict.Add("ProdRight",None)
            dict.Add("String",None)
            dict.Add("StringTuple",None)
            dict.Add("Token",None)
            dict
            )(new Dictionary<string,Option<int>>())
        //s35
        (fun (dict : Dictionary<string,Option<int>>) ->
            dict.Add("BangToken",None)
            dict.Add("Exp",None)
            dict.Add("Group",None)
            dict.Add("Identifier",None)
            dict.Add("Num",None)
            dict.Add("ProdRight",None)
            dict.Add("String",None)
            dict.Add("StringTuple",None)
            dict.Add("Token",None)
            dict
            )(new Dictionary<string,Option<int>>())
        //s36
        (fun (dict : Dictionary<string,Option<int>>) ->
            dict.Add("BangToken",None)
            dict.Add("Exp",None)
            dict.Add("Group",None)
            dict.Add("Identifier",Some 36)
            dict.Add("Num",None)
            dict.Add("ProdRight",Some 45)
            dict.Add("String",Some 38)
            dict.Add("StringTuple",None)
            dict.Add("Token",None)
            dict
            )(new Dictionary<string,Option<int>>())
        //s37
        (fun (dict : Dictionary<string,Option<int>>) ->
            dict.Add("BangToken",None)
            dict.Add("Exp",Some 46)
            dict.Add("Group",None)
            dict.Add("Identifier",None)
            dict.Add("Num",None)
            dict.Add("ProdRight",None)
            dict.Add("String",None)
            dict.Add("StringTuple",None)
            dict.Add("Token",None)
            dict
            )(new Dictionary<string,Option<int>>())
        //s38
        (fun (dict : Dictionary<string,Option<int>>) ->
            dict.Add("BangToken",None)
            dict.Add("Exp",None)
            dict.Add("Group",None)
            dict.Add("Identifier",Some 36)
            dict.Add("Num",None)
            dict.Add("ProdRight",Some 47)
            dict.Add("String",Some 38)
            dict.Add("StringTuple",None)
            dict.Add("Token",None)
            dict
            )(new Dictionary<string,Option<int>>())
        //s39
        (fun (dict : Dictionary<string,Option<int>>) ->
            dict.Add("BangToken",None)
            dict.Add("Exp",None)
            dict.Add("Group",None)
            dict.Add("Identifier",Some 36)
            dict.Add("Num",None)
            dict.Add("ProdRight",Some 48)
            dict.Add("String",Some 38)
            dict.Add("StringTuple",None)
            dict.Add("Token",None)
            dict
            )(new Dictionary<string,Option<int>>())
        //s40
        (fun (dict : Dictionary<string,Option<int>>) ->
            dict.Add("BangToken",None)
            dict.Add("Exp",None)
            dict.Add("Group",None)
            dict.Add("Identifier",None)
            dict.Add("Num",None)
            dict.Add("ProdRight",None)
            dict.Add("String",None)
            dict.Add("StringTuple",None)
            dict.Add("Token",None)
            dict
            )(new Dictionary<string,Option<int>>())
        //s41
        (fun (dict : Dictionary<string,Option<int>>) ->
            dict.Add("BangToken",None)
            dict.Add("Exp",None)
            dict.Add("Group",None)
            dict.Add("Identifier",None)
            dict.Add("Num",None)
            dict.Add("ProdRight",None)
            dict.Add("String",None)
            dict.Add("StringTuple",None)
            dict.Add("Token",None)
            dict
            )(new Dictionary<string,Option<int>>())
        //s42
        (fun (dict : Dictionary<string,Option<int>>) ->
            dict.Add("BangToken",None)
            dict.Add("Exp",None)
            dict.Add("Group",None)
            dict.Add("Identifier",None)
            dict.Add("Num",None)
            dict.Add("ProdRight",None)
            dict.Add("String",Some 16)
            dict.Add("StringTuple",Some 49)
            dict.Add("Token",None)
            dict
            )(new Dictionary<string,Option<int>>())
        //s43
        (fun (dict : Dictionary<string,Option<int>>) ->
            dict.Add("BangToken",None)
            dict.Add("Exp",None)
            dict.Add("Group",None)
            dict.Add("Identifier",None)
            dict.Add("Num",None)
            dict.Add("ProdRight",None)
            dict.Add("String",None)
            dict.Add("StringTuple",None)
            dict.Add("Token",None)
            dict
            )(new Dictionary<string,Option<int>>())
        //s44
        (fun (dict : Dictionary<string,Option<int>>) ->
            dict.Add("BangToken",None)
            dict.Add("Exp",None)
            dict.Add("Group",None)
            dict.Add("Identifier",None)
            dict.Add("Num",None)
            dict.Add("ProdRight",None)
            dict.Add("String",None)
            dict.Add("StringTuple",None)
            dict.Add("Token",None)
            dict
            )(new Dictionary<string,Option<int>>())
        //s45
        (fun (dict : Dictionary<string,Option<int>>) ->
            dict.Add("BangToken",None)
            dict.Add("Exp",None)
            dict.Add("Group",None)
            dict.Add("Identifier",None)
            dict.Add("Num",None)
            dict.Add("ProdRight",None)
            dict.Add("String",None)
            dict.Add("StringTuple",None)
            dict.Add("Token",None)
            dict
            )(new Dictionary<string,Option<int>>())
        //s46
        (fun (dict : Dictionary<string,Option<int>>) ->
            dict.Add("BangToken",None)
            dict.Add("Exp",None)
            dict.Add("Group",None)
            dict.Add("Identifier",None)
            dict.Add("Num",None)
            dict.Add("ProdRight",None)
            dict.Add("String",None)
            dict.Add("StringTuple",None)
            dict.Add("Token",None)
            dict
            )(new Dictionary<string,Option<int>>())
        //s47
        (fun (dict : Dictionary<string,Option<int>>) ->
            dict.Add("BangToken",None)
            dict.Add("Exp",None)
            dict.Add("Group",None)
            dict.Add("Identifier",None)
            dict.Add("Num",None)
            dict.Add("ProdRight",None)
            dict.Add("String",None)
            dict.Add("StringTuple",None)
            dict.Add("Token",None)
            dict
            )(new Dictionary<string,Option<int>>())
        //s48
        (fun (dict : Dictionary<string,Option<int>>) ->
            dict.Add("BangToken",None)
            dict.Add("Exp",None)
            dict.Add("Group",None)
            dict.Add("Identifier",None)
            dict.Add("Num",None)
            dict.Add("ProdRight",None)
            dict.Add("String",None)
            dict.Add("StringTuple",None)
            dict.Add("Token",None)
            dict
            )(new Dictionary<string,Option<int>>())
        //s49
        (fun (dict : Dictionary<string,Option<int>>) ->
            dict.Add("BangToken",None)
            dict.Add("Exp",None)
            dict.Add("Group",None)
            dict.Add("Identifier",None)
            dict.Add("Num",None)
            dict.Add("ProdRight",None)
            dict.Add("String",None)
            dict.Add("StringTuple",None)
            dict.Add("Token",None)
            dict
            )(new Dictionary<string,Option<int>>())
        |]
    let productions_str = [|
        ("__",[|"Exp"|]) //0
        ("Exp",[|"prod";"Identifier";"rarrow";"ProdRight";"Exp"|]) //1
        ("Exp",[|"prec";"String";"Num";"Exp"|]) //2
        ("Exp",[|"assoc";"String";"colon";"StringTuple";"Exp"|]) //3
        ("Exp",[|"token";"Token";"Exp"|]) //4
        ("Exp",[|"bang";"token";"BangToken";"Exp"|]) //5
        ("Exp",[|"tgroup";"Group";"Exp"|]) //6
        ("Exp",[||]) //7
        ("ProdRight",[|"String";"ProdRight"|]) //8
        ("ProdRight",[|"Identifier";"ProdRight"|]) //9
        ("ProdRight",[|"mid";"ProdRight"|]) //10
        ("ProdRight",[||]) //11
        ("Token",[|"StringTuple";"as";"StringTuple"|]) //12
        ("Token",[|"f_cap";"StringTuple";"as";"StringTuple"|]) //13
        ("BangToken",[|"StringTuple"|]) //14
        ("Group",[|"String";"lbrace";"StringTuple";"rbrace"|]) //15
        ("Identifier",[|"identifier"|]) //16
        ("StringTuple",[|"String";"comma";"StringTuple"|]) //17
        ("StringTuple",[|"String"|]) //18
        ("String",[|"string"|]) //19
        ("Num",[|"num"|]) //20
        |]
    let parser tokens =
        let tLen = Array.length tokens
        let popOne = function
            | s::stack -> (s,stack)
            | _ -> failwith "parser: popOne error"
        let popN n stack =
            let rec exec n0 acc = function
                | stack when n0 = n -> (acc,stack) 
                | s::stack -> exec (n0 + 1) (acc @ [s]) stack
                | [] ->
                    failwith "parser: popN error"
            exec 0 [] stack
        let pushGoto stack = function
            | Some g -> g::stack
            | None -> stack
        let getNextFromInput i =
            if i < tLen - 1 then
                (i + 1,tokens.[i+1])
            else failwith "parser: getNextFromInputError"
        let rec exec (i,a) sStack tree pOrder =
            let (s,_) = popOne sStack
            match actionTable.[s].[a|>(fun (a,_,_) -> a)] with
            | Shift t ->
                let newStack = t::sStack
                let newTree = addToken2tree tree a
                let (i,a) = getNextFromInput i
                exec (i,a) newStack newTree pOrder
            | Reduce r ->
                let (prod,rSide,prodF,pos) =
                    let (a0,b0) = productions_str.[r]
                    let f = productions_fun.[r]
                    let p = a|>(fun (_,_,pos) -> pos)
                    (a0,b0,f,p)
                let (newTree,newProdOrder) = prodF tree pOrder pos
                let betaLen = Array.length rSide
                let (_,newStack) = popN betaLen sStack
                let (t,_) = popOne newStack
                let newStack = pushGoto newStack gotoTable.[t].[prod]
                exec (i,a) newStack newTree newProdOrder
            | Accept ->
                let reversedTokenOrder =
                    List.fold
                        (fun _ x ->
                            match x with
                            | DataTokensOrder tOrder -> tOrder.Reverse()
                            | _ -> ()
                            )
                        ()
                        tree
                (tree,pOrder)
            | Error msg ->
                let (x,y) = a|>(fun (_,_,pos) -> pos)
                failwith (sprintf "syntax error(%d,%d): %s" x y msg)
        exec (0,tokens.[0]) [0] initTreeStack []
    let parse str =
        let cleanUp = clear_data()
        let lexed = lexer str
        let parsed = parser lexed
        parsed
