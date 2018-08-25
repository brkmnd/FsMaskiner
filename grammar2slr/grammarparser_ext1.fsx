namespace Maskiner
module GrammarParserExt =
    open System.Collections.Generic
    open System.Text.RegularExpressions
    //Both types are to be produced by the machine. Can be done in groupings.
    type Tokens =
        | Token_term of string
        | Token_nonTerm of string
        | Token_num of string
        | Token_prod | Token_prec | Token_assoc | Token_group | Token_token
        | Token_rarrow | Token_of | Token_comma | Token_mid
        | Token_lbrace | Token_rbrace
        | Token_direction of string
        | Token_dollar
    type Action =
        | Shift of int
        | Reduce of int
        | Accept
        | Error of string
    type ProdExp =
        | NonTerm of string
        | Term of string
        | Dollar
    type DataStruct = {
        prod:Dictionary<string,(ProdExp list) list>
        prec:Dictionary<string,int>
        assoc:Dictionary<string,string>
        group:string list
        }
    type Tree =
        | EmptyTree
        | Node of DataStruct
        | NodeProd of Dictionary<string,(ProdExp list) list>
        | Leaf of ProdExp
        | Acc of ProdExp list
        | AccNested of (ProdExp list) list 
    let lexer inStr =
        let tokensL = new List<Tokens>()
        let addToken (tGroup : GroupCollection) =
            if tGroup.[1].Value <> "" then
                tokensL.Add(Token_term tGroup.[1].Value)
            elif tGroup.[2].Value <> "" then
                tokensL.Add(Token_nonTerm tGroup.[2].Value)
            elif tGroup.[3].Value <> "" then
                tokensL.Add(Token_num tGroup.[3].Value)
            elif tGroup.[4].Value <> "" then
                tokensL.Add(Token_prod)
            elif tGroup.[5].Value <> "" then
                tokensL.Add(Token_prec)
            elif tGroup.[6].Value <> "" then
                tokensL.Add(Token_assoc)
            elif tGroup.[7].Value <> "" then
                tokensL.Add(Token_group)
            elif tGroup.[8].Value <> "" then
                tokensL.Add(Token_token)
            elif tGroup.[9].Value <> "" then
                tokensL.Add(Token_rarrow)
            elif tGroup.[10].Value <> "" then
                tokensL.Add(Token_of)
            elif tGroup.[11].Value <> "" then
                tokensL.Add(Token_comma)
            elif tGroup.[12].Value <> "" then
                tokensL.Add(Token_mid)
            elif tGroup.[13].Value <> "" then
                tokensL.Add(Token_lbrace)
            elif tGroup.[14].Value <> "" then
                tokensL.Add(Token_rbrace)
            elif tGroup.[15].Value <> "" then
                tokensL.Add(Token_direction tGroup.[14].Value)
            else ()
        //reproduce regex by machine
        let regToken =
            "\"([^\"]*)\"|"+
            "([A-Z][a-zA-Z']*)|"+
            "([0-9]+)|"+
            "(prod)|"+
            "(prec)|"+
            "(assoc)|"+
            "(group)|"+
            "(token)|"+
            "(->)|"+
            "(of)|"+
            "(,)|"+
            "(\\|)|"+
            "(\\{)|"+
            "(\\})|"+
            "(left|right)|"+
            "#[^\\n]*\\n|"+
            "[\\n\\t\\r ]+"
        let mfun (m : Match) =
            addToken m.Groups
            ""
        let residueStr = Regex.Replace(inStr,regToken,mfun)
        if residueStr <> "" then
            failwith (sprintf "garbage in expression: %c" residueStr.[0])
        else
            tokensL.Add(Token_dollar)
            Array.init tokensL.Count (fun i -> tokensL.[i])

    let actionTable = [|
        //s0
        (fun (dict : Dictionary<string,Action>) ->
            dict.Add("assoc",Shift 2)
            dict.Add("comma",Error "")
            dict.Add("direction",Error "")
            dict.Add("group",Shift 3)
            dict.Add("lbrace",Error "")
            dict.Add("mid",Error "")
            dict.Add("nonTerm",Error "")
            dict.Add("num",Error "")
            dict.Add("of",Error "")
            dict.Add("prec",Shift 4)
            dict.Add("prod",Shift 5)
            dict.Add("rarrow",Error "")
            dict.Add("rbrace",Error "")
            dict.Add("term",Error "")
            dict.Add("token",Shift 6)
            dict.Add("$",Reduce 6)
            dict
            )(new Dictionary<string,Action>())
        //s1
        (fun (dict : Dictionary<string,Action>) ->
            dict.Add("assoc",Error "")
            dict.Add("comma",Error "")
            dict.Add("direction",Error "")
            dict.Add("group",Error "")
            dict.Add("lbrace",Error "")
            dict.Add("mid",Error "")
            dict.Add("nonTerm",Error "")
            dict.Add("num",Error "")
            dict.Add("of",Error "")
            dict.Add("prec",Error "")
            dict.Add("prod",Error "")
            dict.Add("rarrow",Error "")
            dict.Add("rbrace",Error "")
            dict.Add("term",Error "")
            dict.Add("token",Error "")
            dict.Add("$",Accept)
            dict
            )(new Dictionary<string,Action>())
        //s2
        (fun (dict : Dictionary<string,Action>) ->
            dict.Add("assoc",Error "")
            dict.Add("comma",Error "")
            dict.Add("direction",Error "")
            dict.Add("group",Error "")
            dict.Add("lbrace",Error "")
            dict.Add("mid",Error "")
            dict.Add("nonTerm",Error "")
            dict.Add("num",Error "")
            dict.Add("of",Error "")
            dict.Add("prec",Error "")
            dict.Add("prod",Error "")
            dict.Add("rarrow",Error "")
            dict.Add("rbrace",Error "")
            dict.Add("term",Shift 7)
            dict.Add("token",Error "")
            dict.Add("$",Error "")
            dict
            )(new Dictionary<string,Action>())
        //s3
        (fun (dict : Dictionary<string,Action>) ->
            dict.Add("assoc",Error "")
            dict.Add("comma",Error "")
            dict.Add("direction",Error "")
            dict.Add("group",Error "")
            dict.Add("lbrace",Error "")
            dict.Add("mid",Error "")
            dict.Add("nonTerm",Error "")
            dict.Add("num",Error "")
            dict.Add("of",Error "")
            dict.Add("prec",Error "")
            dict.Add("prod",Error "")
            dict.Add("rarrow",Error "")
            dict.Add("rbrace",Error "")
            dict.Add("term",Shift 9)
            dict.Add("token",Error "")
            dict.Add("$",Error "")
            dict
            )(new Dictionary<string,Action>())
        //s4
        (fun (dict : Dictionary<string,Action>) ->
            dict.Add("assoc",Error "")
            dict.Add("comma",Error "")
            dict.Add("direction",Error "")
            dict.Add("group",Error "")
            dict.Add("lbrace",Error "")
            dict.Add("mid",Error "")
            dict.Add("nonTerm",Error "")
            dict.Add("num",Error "")
            dict.Add("of",Error "")
            dict.Add("prec",Error "")
            dict.Add("prod",Error "")
            dict.Add("rarrow",Error "")
            dict.Add("rbrace",Error "")
            dict.Add("term",Shift 10)
            dict.Add("token",Error "")
            dict.Add("$",Error "")
            dict
            )(new Dictionary<string,Action>())
        //s5
        (fun (dict : Dictionary<string,Action>) ->
            dict.Add("assoc",Error "")
            dict.Add("comma",Error "")
            dict.Add("direction",Error "")
            dict.Add("group",Error "")
            dict.Add("lbrace",Error "")
            dict.Add("mid",Error "")
            dict.Add("nonTerm",Shift 11)
            dict.Add("num",Error "")
            dict.Add("of",Error "")
            dict.Add("prec",Error "")
            dict.Add("prod",Error "")
            dict.Add("rarrow",Error "")
            dict.Add("rbrace",Error "")
            dict.Add("term",Error "")
            dict.Add("token",Error "")
            dict.Add("$",Error "")
            dict
            )(new Dictionary<string,Action>())
        //s6
        (fun (dict : Dictionary<string,Action>) ->
            dict.Add("assoc",Error "")
            dict.Add("comma",Error "")
            dict.Add("direction",Error "")
            dict.Add("group",Error "")
            dict.Add("lbrace",Error "")
            dict.Add("mid",Error "")
            dict.Add("nonTerm",Error "")
            dict.Add("num",Error "")
            dict.Add("of",Shift 13)
            dict.Add("prec",Error "")
            dict.Add("prod",Error "")
            dict.Add("rarrow",Error "")
            dict.Add("rbrace",Error "")
            dict.Add("term",Shift 14)
            dict.Add("token",Error "")
            dict.Add("$",Error "")
            dict
            )(new Dictionary<string,Action>())
        //s7
        (fun (dict : Dictionary<string,Action>) ->
            dict.Add("assoc",Error "")
            dict.Add("comma",Error "")
            dict.Add("direction",Shift 15)
            dict.Add("group",Error "")
            dict.Add("lbrace",Error "")
            dict.Add("mid",Error "")
            dict.Add("nonTerm",Error "")
            dict.Add("num",Error "")
            dict.Add("of",Error "")
            dict.Add("prec",Error "")
            dict.Add("prod",Error "")
            dict.Add("rarrow",Error "")
            dict.Add("rbrace",Error "")
            dict.Add("term",Error "")
            dict.Add("token",Error "")
            dict.Add("$",Error "")
            dict
            )(new Dictionary<string,Action>())
        //s8
        (fun (dict : Dictionary<string,Action>) ->
            dict.Add("assoc",Error "")
            dict.Add("comma",Error "")
            dict.Add("direction",Error "")
            dict.Add("group",Error "")
            dict.Add("lbrace",Error "")
            dict.Add("mid",Error "")
            dict.Add("nonTerm",Error "")
            dict.Add("num",Error "")
            dict.Add("of",Error "")
            dict.Add("prec",Error "")
            dict.Add("prod",Error "")
            dict.Add("rarrow",Error "")
            dict.Add("rbrace",Error "")
            dict.Add("term",Error "")
            dict.Add("token",Error "")
            dict.Add("$",Reduce 5)
            dict
            )(new Dictionary<string,Action>())
        //s9
        (fun (dict : Dictionary<string,Action>) ->
            dict.Add("assoc",Error "")
            dict.Add("comma",Error "")
            dict.Add("direction",Error "")
            dict.Add("group",Error "")
            dict.Add("lbrace",Shift 16)
            dict.Add("mid",Error "")
            dict.Add("nonTerm",Error "")
            dict.Add("num",Error "")
            dict.Add("of",Error "")
            dict.Add("prec",Error "")
            dict.Add("prod",Error "")
            dict.Add("rarrow",Error "")
            dict.Add("rbrace",Error "")
            dict.Add("term",Error "")
            dict.Add("token",Error "")
            dict.Add("$",Error "")
            dict
            )(new Dictionary<string,Action>())
        //s10
        (fun (dict : Dictionary<string,Action>) ->
            dict.Add("assoc",Error "")
            dict.Add("comma",Error "")
            dict.Add("direction",Error "")
            dict.Add("group",Error "")
            dict.Add("lbrace",Error "")
            dict.Add("mid",Error "")
            dict.Add("nonTerm",Error "")
            dict.Add("num",Shift 17)
            dict.Add("of",Error "")
            dict.Add("prec",Error "")
            dict.Add("prod",Error "")
            dict.Add("rarrow",Error "")
            dict.Add("rbrace",Error "")
            dict.Add("term",Error "")
            dict.Add("token",Error "")
            dict.Add("$",Error "")
            dict
            )(new Dictionary<string,Action>())
        //s11
        (fun (dict : Dictionary<string,Action>) ->
            dict.Add("assoc",Error "")
            dict.Add("comma",Error "")
            dict.Add("direction",Error "")
            dict.Add("group",Error "")
            dict.Add("lbrace",Error "")
            dict.Add("mid",Error "")
            dict.Add("nonTerm",Error "")
            dict.Add("num",Error "")
            dict.Add("of",Error "")
            dict.Add("prec",Error "")
            dict.Add("prod",Error "")
            dict.Add("rarrow",Shift 18)
            dict.Add("rbrace",Error "")
            dict.Add("term",Error "")
            dict.Add("token",Error "")
            dict.Add("$",Error "")
            dict
            )(new Dictionary<string,Action>())
        //s12
        (fun (dict : Dictionary<string,Action>) ->
            dict.Add("assoc",Error "")
            dict.Add("comma",Error "")
            dict.Add("direction",Error "")
            dict.Add("group",Error "")
            dict.Add("lbrace",Error "")
            dict.Add("mid",Error "")
            dict.Add("nonTerm",Error "")
            dict.Add("num",Error "")
            dict.Add("of",Error "")
            dict.Add("prec",Error "")
            dict.Add("prod",Error "")
            dict.Add("rarrow",Error "")
            dict.Add("rbrace",Error "")
            dict.Add("term",Error "")
            dict.Add("token",Error "")
            dict.Add("$",Reduce 4)
            dict
            )(new Dictionary<string,Action>())
        //s13
        (fun (dict : Dictionary<string,Action>) ->
            dict.Add("assoc",Error "")
            dict.Add("comma",Error "")
            dict.Add("direction",Error "")
            dict.Add("group",Error "")
            dict.Add("lbrace",Error "")
            dict.Add("mid",Error "")
            dict.Add("nonTerm",Error "")
            dict.Add("num",Error "")
            dict.Add("of",Error "")
            dict.Add("prec",Error "")
            dict.Add("prod",Error "")
            dict.Add("rarrow",Error "")
            dict.Add("rbrace",Error "")
            dict.Add("term",Shift 19)
            dict.Add("token",Error "")
            dict.Add("$",Error "")
            dict
            )(new Dictionary<string,Action>())
        //s14
        (fun (dict : Dictionary<string,Action>) ->
            dict.Add("assoc",Error "")
            dict.Add("comma",Error "")
            dict.Add("direction",Error "")
            dict.Add("group",Error "")
            dict.Add("lbrace",Error "")
            dict.Add("mid",Error "")
            dict.Add("nonTerm",Error "")
            dict.Add("num",Error "")
            dict.Add("of",Error "")
            dict.Add("prec",Error "")
            dict.Add("prod",Error "")
            dict.Add("rarrow",Error "")
            dict.Add("rbrace",Error "")
            dict.Add("term",Error "")
            dict.Add("token",Error "")
            dict.Add("$",Reduce 12)
            dict
            )(new Dictionary<string,Action>())
        //s15
        (fun (dict : Dictionary<string,Action>) ->
            dict.Add("assoc",Shift 2)
            dict.Add("comma",Error "")
            dict.Add("direction",Error "")
            dict.Add("group",Shift 3)
            dict.Add("lbrace",Error "")
            dict.Add("mid",Error "")
            dict.Add("nonTerm",Error "")
            dict.Add("num",Error "")
            dict.Add("of",Error "")
            dict.Add("prec",Shift 4)
            dict.Add("prod",Shift 5)
            dict.Add("rarrow",Error "")
            dict.Add("rbrace",Error "")
            dict.Add("term",Error "")
            dict.Add("token",Shift 6)
            dict.Add("$",Reduce 6)
            dict
            )(new Dictionary<string,Action>())
        //s16
        (fun (dict : Dictionary<string,Action>) ->
            dict.Add("assoc",Error "")
            dict.Add("comma",Error "")
            dict.Add("direction",Error "")
            dict.Add("group",Error "")
            dict.Add("lbrace",Error "")
            dict.Add("mid",Error "")
            dict.Add("nonTerm",Error "")
            dict.Add("num",Error "")
            dict.Add("of",Error "")
            dict.Add("prec",Error "")
            dict.Add("prod",Error "")
            dict.Add("rarrow",Error "")
            dict.Add("rbrace",Error "")
            dict.Add("term",Shift 22)
            dict.Add("token",Error "")
            dict.Add("$",Error "")
            dict
            )(new Dictionary<string,Action>())
        //s17
        (fun (dict : Dictionary<string,Action>) ->
            dict.Add("assoc",Shift 2)
            dict.Add("comma",Error "")
            dict.Add("direction",Error "")
            dict.Add("group",Shift 3)
            dict.Add("lbrace",Error "")
            dict.Add("mid",Error "")
            dict.Add("nonTerm",Error "")
            dict.Add("num",Error "")
            dict.Add("of",Error "")
            dict.Add("prec",Shift 4)
            dict.Add("prod",Shift 5)
            dict.Add("rarrow",Error "")
            dict.Add("rbrace",Error "")
            dict.Add("term",Error "")
            dict.Add("token",Shift 6)
            dict.Add("$",Reduce 6)
            dict
            )(new Dictionary<string,Action>())
        //s18
        (fun (dict : Dictionary<string,Action>) ->
            dict.Add("assoc",Reduce 10)
            dict.Add("comma",Error "")
            dict.Add("direction",Error "")
            dict.Add("group",Reduce 10)
            dict.Add("lbrace",Error "")
            dict.Add("mid",Shift 25)
            dict.Add("nonTerm",Shift 26)
            dict.Add("num",Error "")
            dict.Add("of",Error "")
            dict.Add("prec",Reduce 10)
            dict.Add("prod",Reduce 10)
            dict.Add("rarrow",Error "")
            dict.Add("rbrace",Error "")
            dict.Add("term",Shift 27)
            dict.Add("token",Reduce 10)
            dict.Add("$",Reduce 10)
            dict
            )(new Dictionary<string,Action>())
        //s19
        (fun (dict : Dictionary<string,Action>) ->
            dict.Add("assoc",Error "")
            dict.Add("comma",Error "")
            dict.Add("direction",Error "")
            dict.Add("group",Error "")
            dict.Add("lbrace",Error "")
            dict.Add("mid",Error "")
            dict.Add("nonTerm",Error "")
            dict.Add("num",Error "")
            dict.Add("of",Error "")
            dict.Add("prec",Error "")
            dict.Add("prod",Error "")
            dict.Add("rarrow",Error "")
            dict.Add("rbrace",Error "")
            dict.Add("term",Error "")
            dict.Add("token",Error "")
            dict.Add("$",Reduce 11)
            dict
            )(new Dictionary<string,Action>())
        //s20
        (fun (dict : Dictionary<string,Action>) ->
            dict.Add("assoc",Error "")
            dict.Add("comma",Error "")
            dict.Add("direction",Error "")
            dict.Add("group",Error "")
            dict.Add("lbrace",Error "")
            dict.Add("mid",Error "")
            dict.Add("nonTerm",Error "")
            dict.Add("num",Error "")
            dict.Add("of",Error "")
            dict.Add("prec",Error "")
            dict.Add("prod",Error "")
            dict.Add("rarrow",Error "")
            dict.Add("rbrace",Error "")
            dict.Add("term",Error "")
            dict.Add("token",Error "")
            dict.Add("$",Reduce 3)
            dict
            )(new Dictionary<string,Action>())
        //s21
        (fun (dict : Dictionary<string,Action>) ->
            dict.Add("assoc",Error "")
            dict.Add("comma",Error "")
            dict.Add("direction",Error "")
            dict.Add("group",Error "")
            dict.Add("lbrace",Error "")
            dict.Add("mid",Error "")
            dict.Add("nonTerm",Error "")
            dict.Add("num",Error "")
            dict.Add("of",Error "")
            dict.Add("prec",Error "")
            dict.Add("prod",Error "")
            dict.Add("rarrow",Error "")
            dict.Add("rbrace",Shift 28)
            dict.Add("term",Error "")
            dict.Add("token",Error "")
            dict.Add("$",Error "")
            dict
            )(new Dictionary<string,Action>())
        //s22
        (fun (dict : Dictionary<string,Action>) ->
            dict.Add("assoc",Error "")
            dict.Add("comma",Shift 29)
            dict.Add("direction",Error "")
            dict.Add("group",Error "")
            dict.Add("lbrace",Error "")
            dict.Add("mid",Error "")
            dict.Add("nonTerm",Error "")
            dict.Add("num",Error "")
            dict.Add("of",Error "")
            dict.Add("prec",Error "")
            dict.Add("prod",Error "")
            dict.Add("rarrow",Error "")
            dict.Add("rbrace",Reduce 15)
            dict.Add("term",Error "")
            dict.Add("token",Error "")
            dict.Add("$",Error "")
            dict
            )(new Dictionary<string,Action>())
        //s23
        (fun (dict : Dictionary<string,Action>) ->
            dict.Add("assoc",Error "")
            dict.Add("comma",Error "")
            dict.Add("direction",Error "")
            dict.Add("group",Error "")
            dict.Add("lbrace",Error "")
            dict.Add("mid",Error "")
            dict.Add("nonTerm",Error "")
            dict.Add("num",Error "")
            dict.Add("of",Error "")
            dict.Add("prec",Error "")
            dict.Add("prod",Error "")
            dict.Add("rarrow",Error "")
            dict.Add("rbrace",Error "")
            dict.Add("term",Error "")
            dict.Add("token",Error "")
            dict.Add("$",Reduce 2)
            dict
            )(new Dictionary<string,Action>())
        //s24
        (fun (dict : Dictionary<string,Action>) ->
            dict.Add("assoc",Shift 2)
            dict.Add("comma",Error "")
            dict.Add("direction",Error "")
            dict.Add("group",Shift 3)
            dict.Add("lbrace",Error "")
            dict.Add("mid",Error "")
            dict.Add("nonTerm",Error "")
            dict.Add("num",Error "")
            dict.Add("of",Error "")
            dict.Add("prec",Shift 4)
            dict.Add("prod",Shift 5)
            dict.Add("rarrow",Error "")
            dict.Add("rbrace",Error "")
            dict.Add("term",Error "")
            dict.Add("token",Shift 6)
            dict.Add("$",Reduce 6)
            dict
            )(new Dictionary<string,Action>())
        //s25
        (fun (dict : Dictionary<string,Action>) ->
            dict.Add("assoc",Reduce 10)
            dict.Add("comma",Error "")
            dict.Add("direction",Error "")
            dict.Add("group",Reduce 10)
            dict.Add("lbrace",Error "")
            dict.Add("mid",Shift 25)
            dict.Add("nonTerm",Shift 26)
            dict.Add("num",Error "")
            dict.Add("of",Error "")
            dict.Add("prec",Reduce 10)
            dict.Add("prod",Reduce 10)
            dict.Add("rarrow",Error "")
            dict.Add("rbrace",Error "")
            dict.Add("term",Shift 27)
            dict.Add("token",Reduce 10)
            dict.Add("$",Reduce 10)
            dict
            )(new Dictionary<string,Action>())
        //s26
        (fun (dict : Dictionary<string,Action>) ->
            dict.Add("assoc",Reduce 10)
            dict.Add("comma",Error "")
            dict.Add("direction",Error "")
            dict.Add("group",Reduce 10)
            dict.Add("lbrace",Error "")
            dict.Add("mid",Shift 25)
            dict.Add("nonTerm",Shift 26)
            dict.Add("num",Error "")
            dict.Add("of",Error "")
            dict.Add("prec",Reduce 10)
            dict.Add("prod",Reduce 10)
            dict.Add("rarrow",Error "")
            dict.Add("rbrace",Error "")
            dict.Add("term",Shift 27)
            dict.Add("token",Reduce 10)
            dict.Add("$",Reduce 10)
            dict
            )(new Dictionary<string,Action>())
        //s27
        (fun (dict : Dictionary<string,Action>) ->
            dict.Add("assoc",Reduce 10)
            dict.Add("comma",Error "")
            dict.Add("direction",Error "")
            dict.Add("group",Reduce 10)
            dict.Add("lbrace",Error "")
            dict.Add("mid",Shift 25)
            dict.Add("nonTerm",Shift 26)
            dict.Add("num",Error "")
            dict.Add("of",Error "")
            dict.Add("prec",Reduce 10)
            dict.Add("prod",Reduce 10)
            dict.Add("rarrow",Error "")
            dict.Add("rbrace",Error "")
            dict.Add("term",Shift 27)
            dict.Add("token",Reduce 10)
            dict.Add("$",Reduce 10)
            dict
            )(new Dictionary<string,Action>())
        //s28
        (fun (dict : Dictionary<string,Action>) ->
            dict.Add("assoc",Error "")
            dict.Add("comma",Error "")
            dict.Add("direction",Error "")
            dict.Add("group",Error "")
            dict.Add("lbrace",Error "")
            dict.Add("mid",Error "")
            dict.Add("nonTerm",Error "")
            dict.Add("num",Error "")
            dict.Add("of",Error "")
            dict.Add("prec",Error "")
            dict.Add("prod",Error "")
            dict.Add("rarrow",Error "")
            dict.Add("rbrace",Error "")
            dict.Add("term",Error "")
            dict.Add("token",Error "")
            dict.Add("$",Reduce 13)
            dict
            )(new Dictionary<string,Action>())
        //s29
        (fun (dict : Dictionary<string,Action>) ->
            dict.Add("assoc",Error "")
            dict.Add("comma",Error "")
            dict.Add("direction",Error "")
            dict.Add("group",Error "")
            dict.Add("lbrace",Error "")
            dict.Add("mid",Error "")
            dict.Add("nonTerm",Error "")
            dict.Add("num",Error "")
            dict.Add("of",Error "")
            dict.Add("prec",Error "")
            dict.Add("prod",Error "")
            dict.Add("rarrow",Error "")
            dict.Add("rbrace",Error "")
            dict.Add("term",Shift 22)
            dict.Add("token",Error "")
            dict.Add("$",Error "")
            dict
            )(new Dictionary<string,Action>())
        //s30
        (fun (dict : Dictionary<string,Action>) ->
            dict.Add("assoc",Error "")
            dict.Add("comma",Error "")
            dict.Add("direction",Error "")
            dict.Add("group",Error "")
            dict.Add("lbrace",Error "")
            dict.Add("mid",Error "")
            dict.Add("nonTerm",Error "")
            dict.Add("num",Error "")
            dict.Add("of",Error "")
            dict.Add("prec",Error "")
            dict.Add("prod",Error "")
            dict.Add("rarrow",Error "")
            dict.Add("rbrace",Error "")
            dict.Add("term",Error "")
            dict.Add("token",Error "")
            dict.Add("$",Reduce 1)
            dict
            )(new Dictionary<string,Action>())
        //s31
        (fun (dict : Dictionary<string,Action>) ->
            dict.Add("assoc",Reduce 9)
            dict.Add("comma",Error "")
            dict.Add("direction",Error "")
            dict.Add("group",Reduce 9)
            dict.Add("lbrace",Error "")
            dict.Add("mid",Error "")
            dict.Add("nonTerm",Error "")
            dict.Add("num",Error "")
            dict.Add("of",Error "")
            dict.Add("prec",Reduce 9)
            dict.Add("prod",Reduce 9)
            dict.Add("rarrow",Error "")
            dict.Add("rbrace",Error "")
            dict.Add("term",Error "")
            dict.Add("token",Reduce 9)
            dict.Add("$",Reduce 9)
            dict
            )(new Dictionary<string,Action>())
        //s32
        (fun (dict : Dictionary<string,Action>) ->
            dict.Add("assoc",Reduce 8)
            dict.Add("comma",Error "")
            dict.Add("direction",Error "")
            dict.Add("group",Reduce 8)
            dict.Add("lbrace",Error "")
            dict.Add("mid",Error "")
            dict.Add("nonTerm",Error "")
            dict.Add("num",Error "")
            dict.Add("of",Error "")
            dict.Add("prec",Reduce 8)
            dict.Add("prod",Reduce 8)
            dict.Add("rarrow",Error "")
            dict.Add("rbrace",Error "")
            dict.Add("term",Error "")
            dict.Add("token",Reduce 8)
            dict.Add("$",Reduce 8)
            dict
            )(new Dictionary<string,Action>())
        //s33
        (fun (dict : Dictionary<string,Action>) ->
            dict.Add("assoc",Reduce 7)
            dict.Add("comma",Error "")
            dict.Add("direction",Error "")
            dict.Add("group",Reduce 7)
            dict.Add("lbrace",Error "")
            dict.Add("mid",Error "")
            dict.Add("nonTerm",Error "")
            dict.Add("num",Error "")
            dict.Add("of",Error "")
            dict.Add("prec",Reduce 7)
            dict.Add("prod",Reduce 7)
            dict.Add("rarrow",Error "")
            dict.Add("rbrace",Error "")
            dict.Add("term",Error "")
            dict.Add("token",Reduce 7)
            dict.Add("$",Reduce 7)
            dict
            )(new Dictionary<string,Action>())
        //s34
        (fun (dict : Dictionary<string,Action>) ->
            dict.Add("assoc",Error "")
            dict.Add("comma",Error "")
            dict.Add("direction",Error "")
            dict.Add("group",Error "")
            dict.Add("lbrace",Error "")
            dict.Add("mid",Error "")
            dict.Add("nonTerm",Error "")
            dict.Add("num",Error "")
            dict.Add("of",Error "")
            dict.Add("prec",Error "")
            dict.Add("prod",Error "")
            dict.Add("rarrow",Error "")
            dict.Add("rbrace",Reduce 14)
            dict.Add("term",Error "")
            dict.Add("token",Error "")
            dict.Add("$",Error "")
            dict
            )(new Dictionary<string,Action>())
        |]
    let gotoTable = [|
        //s0
        (fun (dict : Dictionary<string,Option<int>>) ->
            dict.Add("Exp",Some 1)
            dict.Add("Group",None)
            dict.Add("GroupExp",None)
            dict.Add("ProdRight",None)
            dict.Add("Token",None)
            dict
            )(new Dictionary<string,Option<int>>())
        //s1
        (fun (dict : Dictionary<string,Option<int>>) ->
            dict.Add("Exp",None)
            dict.Add("Group",None)
            dict.Add("GroupExp",None)
            dict.Add("ProdRight",None)
            dict.Add("Token",None)
            dict
            )(new Dictionary<string,Option<int>>())
        //s2
        (fun (dict : Dictionary<string,Option<int>>) ->
            dict.Add("Exp",None)
            dict.Add("Group",None)
            dict.Add("GroupExp",None)
            dict.Add("ProdRight",None)
            dict.Add("Token",None)
            dict
            )(new Dictionary<string,Option<int>>())
        //s3
        (fun (dict : Dictionary<string,Option<int>>) ->
            dict.Add("Exp",None)
            dict.Add("Group",Some 8)
            dict.Add("GroupExp",None)
            dict.Add("ProdRight",None)
            dict.Add("Token",None)
            dict
            )(new Dictionary<string,Option<int>>())
        //s4
        (fun (dict : Dictionary<string,Option<int>>) ->
            dict.Add("Exp",None)
            dict.Add("Group",None)
            dict.Add("GroupExp",None)
            dict.Add("ProdRight",None)
            dict.Add("Token",None)
            dict
            )(new Dictionary<string,Option<int>>())
        //s5
        (fun (dict : Dictionary<string,Option<int>>) ->
            dict.Add("Exp",None)
            dict.Add("Group",None)
            dict.Add("GroupExp",None)
            dict.Add("ProdRight",None)
            dict.Add("Token",None)
            dict
            )(new Dictionary<string,Option<int>>())
        //s6
        (fun (dict : Dictionary<string,Option<int>>) ->
            dict.Add("Exp",None)
            dict.Add("Group",None)
            dict.Add("GroupExp",None)
            dict.Add("ProdRight",None)
            dict.Add("Token",Some 12)
            dict
            )(new Dictionary<string,Option<int>>())
        //s7
        (fun (dict : Dictionary<string,Option<int>>) ->
            dict.Add("Exp",None)
            dict.Add("Group",None)
            dict.Add("GroupExp",None)
            dict.Add("ProdRight",None)
            dict.Add("Token",None)
            dict
            )(new Dictionary<string,Option<int>>())
        //s8
        (fun (dict : Dictionary<string,Option<int>>) ->
            dict.Add("Exp",None)
            dict.Add("Group",None)
            dict.Add("GroupExp",None)
            dict.Add("ProdRight",None)
            dict.Add("Token",None)
            dict
            )(new Dictionary<string,Option<int>>())
        //s9
        (fun (dict : Dictionary<string,Option<int>>) ->
            dict.Add("Exp",None)
            dict.Add("Group",None)
            dict.Add("GroupExp",None)
            dict.Add("ProdRight",None)
            dict.Add("Token",None)
            dict
            )(new Dictionary<string,Option<int>>())
        //s10
        (fun (dict : Dictionary<string,Option<int>>) ->
            dict.Add("Exp",None)
            dict.Add("Group",None)
            dict.Add("GroupExp",None)
            dict.Add("ProdRight",None)
            dict.Add("Token",None)
            dict
            )(new Dictionary<string,Option<int>>())
        //s11
        (fun (dict : Dictionary<string,Option<int>>) ->
            dict.Add("Exp",None)
            dict.Add("Group",None)
            dict.Add("GroupExp",None)
            dict.Add("ProdRight",None)
            dict.Add("Token",None)
            dict
            )(new Dictionary<string,Option<int>>())
        //s12
        (fun (dict : Dictionary<string,Option<int>>) ->
            dict.Add("Exp",None)
            dict.Add("Group",None)
            dict.Add("GroupExp",None)
            dict.Add("ProdRight",None)
            dict.Add("Token",None)
            dict
            )(new Dictionary<string,Option<int>>())
        //s13
        (fun (dict : Dictionary<string,Option<int>>) ->
            dict.Add("Exp",None)
            dict.Add("Group",None)
            dict.Add("GroupExp",None)
            dict.Add("ProdRight",None)
            dict.Add("Token",None)
            dict
            )(new Dictionary<string,Option<int>>())
        //s14
        (fun (dict : Dictionary<string,Option<int>>) ->
            dict.Add("Exp",None)
            dict.Add("Group",None)
            dict.Add("GroupExp",None)
            dict.Add("ProdRight",None)
            dict.Add("Token",None)
            dict
            )(new Dictionary<string,Option<int>>())
        //s15
        (fun (dict : Dictionary<string,Option<int>>) ->
            dict.Add("Exp",Some 20)
            dict.Add("Group",None)
            dict.Add("GroupExp",None)
            dict.Add("ProdRight",None)
            dict.Add("Token",None)
            dict
            )(new Dictionary<string,Option<int>>())
        //s16
        (fun (dict : Dictionary<string,Option<int>>) ->
            dict.Add("Exp",None)
            dict.Add("Group",None)
            dict.Add("GroupExp",Some 21)
            dict.Add("ProdRight",None)
            dict.Add("Token",None)
            dict
            )(new Dictionary<string,Option<int>>())
        //s17
        (fun (dict : Dictionary<string,Option<int>>) ->
            dict.Add("Exp",Some 23)
            dict.Add("Group",None)
            dict.Add("GroupExp",None)
            dict.Add("ProdRight",None)
            dict.Add("Token",None)
            dict
            )(new Dictionary<string,Option<int>>())
        //s18
        (fun (dict : Dictionary<string,Option<int>>) ->
            dict.Add("Exp",None)
            dict.Add("Group",None)
            dict.Add("GroupExp",None)
            dict.Add("ProdRight",Some 24)
            dict.Add("Token",None)
            dict
            )(new Dictionary<string,Option<int>>())
        //s19
        (fun (dict : Dictionary<string,Option<int>>) ->
            dict.Add("Exp",None)
            dict.Add("Group",None)
            dict.Add("GroupExp",None)
            dict.Add("ProdRight",None)
            dict.Add("Token",None)
            dict
            )(new Dictionary<string,Option<int>>())
        //s20
        (fun (dict : Dictionary<string,Option<int>>) ->
            dict.Add("Exp",None)
            dict.Add("Group",None)
            dict.Add("GroupExp",None)
            dict.Add("ProdRight",None)
            dict.Add("Token",None)
            dict
            )(new Dictionary<string,Option<int>>())
        //s21
        (fun (dict : Dictionary<string,Option<int>>) ->
            dict.Add("Exp",None)
            dict.Add("Group",None)
            dict.Add("GroupExp",None)
            dict.Add("ProdRight",None)
            dict.Add("Token",None)
            dict
            )(new Dictionary<string,Option<int>>())
        //s22
        (fun (dict : Dictionary<string,Option<int>>) ->
            dict.Add("Exp",None)
            dict.Add("Group",None)
            dict.Add("GroupExp",None)
            dict.Add("ProdRight",None)
            dict.Add("Token",None)
            dict
            )(new Dictionary<string,Option<int>>())
        //s23
        (fun (dict : Dictionary<string,Option<int>>) ->
            dict.Add("Exp",None)
            dict.Add("Group",None)
            dict.Add("GroupExp",None)
            dict.Add("ProdRight",None)
            dict.Add("Token",None)
            dict
            )(new Dictionary<string,Option<int>>())
        //s24
        (fun (dict : Dictionary<string,Option<int>>) ->
            dict.Add("Exp",Some 30)
            dict.Add("Group",None)
            dict.Add("GroupExp",None)
            dict.Add("ProdRight",None)
            dict.Add("Token",None)
            dict
            )(new Dictionary<string,Option<int>>())
        //s25
        (fun (dict : Dictionary<string,Option<int>>) ->
            dict.Add("Exp",None)
            dict.Add("Group",None)
            dict.Add("GroupExp",None)
            dict.Add("ProdRight",Some 31)
            dict.Add("Token",None)
            dict
            )(new Dictionary<string,Option<int>>())
        //s26
        (fun (dict : Dictionary<string,Option<int>>) ->
            dict.Add("Exp",None)
            dict.Add("Group",None)
            dict.Add("GroupExp",None)
            dict.Add("ProdRight",Some 32)
            dict.Add("Token",None)
            dict
            )(new Dictionary<string,Option<int>>())
        //s27
        (fun (dict : Dictionary<string,Option<int>>) ->
            dict.Add("Exp",None)
            dict.Add("Group",None)
            dict.Add("GroupExp",None)
            dict.Add("ProdRight",Some 33)
            dict.Add("Token",None)
            dict
            )(new Dictionary<string,Option<int>>())
        //s28
        (fun (dict : Dictionary<string,Option<int>>) ->
            dict.Add("Exp",None)
            dict.Add("Group",None)
            dict.Add("GroupExp",None)
            dict.Add("ProdRight",None)
            dict.Add("Token",None)
            dict
            )(new Dictionary<string,Option<int>>())
        //s29
        (fun (dict : Dictionary<string,Option<int>>) ->
            dict.Add("Exp",None)
            dict.Add("Group",None)
            dict.Add("GroupExp",Some 34)
            dict.Add("ProdRight",None)
            dict.Add("Token",None)
            dict
            )(new Dictionary<string,Option<int>>())
        //s30
        (fun (dict : Dictionary<string,Option<int>>) ->
            dict.Add("Exp",None)
            dict.Add("Group",None)
            dict.Add("GroupExp",None)
            dict.Add("ProdRight",None)
            dict.Add("Token",None)
            dict
            )(new Dictionary<string,Option<int>>())
        //s31
        (fun (dict : Dictionary<string,Option<int>>) ->
            dict.Add("Exp",None)
            dict.Add("Group",None)
            dict.Add("GroupExp",None)
            dict.Add("ProdRight",None)
            dict.Add("Token",None)
            dict
            )(new Dictionary<string,Option<int>>())
        //s32
        (fun (dict : Dictionary<string,Option<int>>) ->
            dict.Add("Exp",None)
            dict.Add("Group",None)
            dict.Add("GroupExp",None)
            dict.Add("ProdRight",None)
            dict.Add("Token",None)
            dict
            )(new Dictionary<string,Option<int>>())
        //s33
        (fun (dict : Dictionary<string,Option<int>>) ->
            dict.Add("Exp",None)
            dict.Add("Group",None)
            dict.Add("GroupExp",None)
            dict.Add("ProdRight",None)
            dict.Add("Token",None)
            dict
            )(new Dictionary<string,Option<int>>())
        //s34
        (fun (dict : Dictionary<string,Option<int>>) ->
            dict.Add("Exp",None)
            dict.Add("Group",None)
            dict.Add("GroupExp",None)
            dict.Add("ProdRight",None)
            dict.Add("Token",None)
            dict
            )(new Dictionary<string,Option<int>>())
        |]
    let productions_str = [|
        ("__",[|"Exp"|]) //0
        ("Exp",[|"prod";"nonTerm";"rarrow";"ProdRight";"Exp"|]) //1
        ("Exp",[|"prec";"term";"num";"Exp"|]) //2
        ("Exp",[|"assoc";"term";"direction";"Exp"|]) //3
        ("Exp",[|"token";"Token"|]) //4
        ("Exp",[|"group";"Group"|]) //5
        ("Exp",[||]) //6
        ("ProdRight",[|"term";"ProdRight"|]) //7
        ("ProdRight",[|"nonTerm";"ProdRight"|]) //8
        ("ProdRight",[|"mid";"ProdRight"|]) //9
        ("ProdRight",[||]) //10
        ("Token",[|"of";"term"|]) //11
        ("Token",[|"term"|]) //12
        ("Group",[|"term";"lbrace";"GroupExp";"rbrace"|]) //13
        ("GroupExp",[|"term";"comma";"GroupExp"|]) //14
        ("GroupExp",[|"term"|]) //15
        |]
    let productions_fun = [|
        //[0] __ -> Exp 
        (fun tree -> tree)
        //[1] Exp -> prod nonTerm rarrow ProdRight Exp 
        (fun tree ->
            match tree with
            | (Acc rSide)::(Leaf (NonTerm pName))::tree ->
                printfn "prod : %s -> %A" pName rSide
                tree
            | (Leaf (NonTerm pName))::rSide::tree ->
                printfn "prod %s -> %A" pName rSide
                tree
            | _ -> tree
            )
        //[2] Exp -> prec term num Exp 
        (fun tree ->
            match tree with
            | (Acc acc)::tree ->
                printfn "prec : %A" acc
                tree
            | _ -> tree
            )
        //[3] Exp -> assoc term direction Exp 
        (fun tree -> tree)
        //[4] Exp -> token Token 
        (fun tree ->
            match tree with
            | (Leaf (Term tName))::(Leaf (Term rx))::tree ->
                printfn "token %s of %s" tName rx
                tree
            | _ -> tree
            )
        //[5] Exp -> group Group 
        (fun tree -> tree)
        //[6] Exp -> 
        (fun tree -> tree)
        //[7] ProdRight -> term ProdRight 
        (fun tree ->
            match tree with
            | (Acc acc)::(Leaf pExp)::tree ->
                (Acc (pExp::acc))::tree
            | (Leaf pExp)::tree ->
                (Acc [pExp])::tree
            | _ -> tree
            )
        //[8] ProdRight -> nonTerm ProdRight 
        (fun tree ->
            match tree with
            | (Acc acc)::(Leaf pExp)::tree ->
                (Acc (pExp::acc))::tree
            | (Leaf pExp)::tree ->
                (Acc [pExp])::tree
            | _ -> tree
            )
        //[9] ProdRight -> mid ProdRight 
        (fun tree ->
            match tree with
            | (Acc acc)::tree ->
                printfn "mid : %A" acc
                tree
            | _ ->
                printfn "mid : %A" tree.[0]
                tree
            )
        //[10] ProdRight -> 
        (fun tree ->
            match tree with
            | (Acc acc)::tree ->
                printfn "ProdRight e : %A" acc
                tree
            | _ ->
                
                tree
            )
        //[11] Token -> of term 
        (fun tree -> tree)
        //[12] Token -> term 
        (fun tree -> tree)
        //[13] Group -> term lbrace GroupExp rbrace 
        (fun tree -> tree)
        //[14] GroupExp -> term comma GroupExp 
        (fun tree -> tree)
        //[15] GroupExp -> term 
        (fun tree -> tree)
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
        //Are to be produced by the converter 
        let token2lookup = function
            | Token_term _ -> "term"
            | Token_nonTerm _ -> "nonTerm"
            | Token_num _ -> "num"
            | Token_prod -> "prod"
            | Token_prec -> "prec"
            | Token_assoc -> "assoc"
            | Token_group -> "group"
            | Token_token -> "token"
            | Token_rarrow -> "rarrow"
            | Token_of -> "of"
            | Token_comma -> "comma"
            | Token_mid -> "mid"
            | Token_lbrace -> "lbrace"
            | Token_rbrace -> "rbrace"
            | Token_direction _ -> "direction"
            | Token_dollar -> "$"
        //Dont know yet
        let addAtom2tree tree node =
            match node with
            | Token_term t -> (Leaf (Term t))::tree
            | Token_nonTerm t -> (Leaf (NonTerm t))::tree
            //| ((Acc acc)::tree,Token_nonTerm t) ->
            //    (Acc ((NonTerm t)::acc))::tree
            //| (tree,Token_nonTerm t) ->
            //    (Acc [NonTerm t])::tree
            //| Token_nonTerm t -> (Leaf t)::tree
            //| Token_num n -> (Leaf n)::tree
            | _ -> tree
        let rec exec (i,a) sStack tree =
            let (s,_) = popOne sStack
            match actionTable.[s].[token2lookup a] with
            | Shift t ->
                let newStack = t::sStack
                let newTree = addAtom2tree tree a
                let (i,a) = getNextFromInput i
                exec (i,a) newStack newTree
            | Reduce r ->
                let (prod,rSide,prodF) =
                    let (a,b) = productions_str.[r]
                    let f = productions_fun.[r]
                    (a,b,f)
                let newTree = prodF tree
                let betaLen = Array.length rSide
                let (_,newStack) = popN betaLen sStack
                let (t,_) = popOne newStack
                let newStack = pushGoto newStack gotoTable.[t].[prod]
                exec (i,a) newStack newTree
            | Accept -> tree
            | Error msg ->
                failwith (sprintf "syntax error: %s" msg)

        let d = NodeProd (new Dictionary<string,(ProdExp list) list>())
        exec (0,tokens.[0]) [0] [d]
    let parse str =
        let lexed = lexer str
        let parsed = parser lexed
        parsed

