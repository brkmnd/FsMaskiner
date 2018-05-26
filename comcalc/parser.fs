namespace Maskiner
module MathParser =
    open System.Collections.Generic
    open System.Text.RegularExpressions
    type Token =
        | Int of string
        | Float of string
        | Id of string
        | Comma
        | Lpar | Rpar
        | Plus | Minus
        | Times | Divide
        | Power
        | Dollar
    type Action =
        | Shift of int
        | Reduce of int
        | Accept
        | Error of string
    type Tree =
        | EmptyTree
        | ArgResult of float
        | Result of float
        | FuncId of string
        | ConstResult of float
    let lexer inStr =
        let tokensL = new List<Token>()
        let addToken (tGroup : GroupCollection) =
            if tGroup.[1].Value <> "" then
                tokensL.Add(Plus)
            elif tGroup.[2].Value <> "" then
                tokensL.Add(Minus)
            elif tGroup.[3].Value <> "" then
                tokensL.Add(Times)
            elif tGroup.[4].Value <> "" then
                tokensL.Add(Divide)
            elif tGroup.[5].Value <> "" then
                tokensL.Add(Power)
            elif tGroup.[6].Value <> "" then
                tokensL.Add(Lpar)
            elif tGroup.[7].Value <> "" then
                tokensL.Add(Rpar)
            elif tGroup.[8].Value <> "" then
                tokensL.Add(Id tGroup.[8].Value)
            elif tGroup.[9].Value <> "" then
                tokensL.Add(Float tGroup.[9].Value)
            elif tGroup.[10].Value <> "" then
                tokensL.Add(Int tGroup.[10].Value)
            elif tGroup.[11].Value <> "" then
                tokensL.Add(Comma)
            else ()
        let regToken =
            "(\\+)|"+
            "(\\-)|"+
            "(\\*)|"+
            "(\\/)|"+
            "(\\^)|"+
            "(\\()|"+
            "(\\))|"+
            "([a-zA-Z]+[a-zA-Z.]*)|"+
            "([0-9]*\\.[0-9+])|"+
            "([0-9]+)|"+
            "(,)|"+
            "[\\n\\t\\r ]+"
        let mfun (m : Match) =
            addToken m.Groups
            ""
        let residueStr = Regex.Replace(inStr,regToken,mfun)
        if residueStr <> "" then
            failwith (sprintf "garbage in expression: %c" residueStr.[0])
        else
            tokensL.Add(Dollar)
            Array.init tokensL.Count (fun i -> tokensL.[i])
    let e_op_exp = Error "given expression, but expected operator"
    let e_op_eoi = Error "reached end of input, but expected operator/missing )"
    let e_exp_op = Error "given operator, but expected expression" 
    let e_exp_eoi = Error "reached end of input, but expected expression"
    let e_args_any = Error "expteded a tuple of arguments"
    let e_args_end = Error "unclosed argument tuple"
    let e_mis_lpar = Error "misplaced ("
    let e_mis_rpar = Error "misplaced )"
    let e_mis_comma = Error "misplaced ,"
    let actionTable = [|
        //s0
        (fun (dict : Dictionary<Token,Action>) ->
            dict.Add(Int "",Shift 6)
            dict.Add(Float "",Shift 4)
            dict.Add(Id "",Shift 5)
            dict.Add(Plus,e_exp_op)
            dict.Add(Minus,Shift 8)
            dict.Add(Times,e_exp_op)
            dict.Add(Divide,e_exp_op)
            dict.Add(Power,e_exp_op)
            dict.Add(Lpar,Shift 7)
            dict.Add(Rpar,e_mis_rpar)
            dict.Add(Comma,e_exp_op)
            dict.Add(Dollar,e_exp_op)
            dict
            )(new Dictionary<Token,Action>())
        //s1
        (fun (dict : Dictionary<Token,Action>) ->
            dict.Add(Int "",e_op_exp)
            dict.Add(Float "",e_op_exp)
            dict.Add(Id "",e_op_exp)
            dict.Add(Plus,Reduce 7)
            dict.Add(Minus,Reduce 7)
            dict.Add(Times,Reduce 7)
            dict.Add(Divide,Reduce 7)
            dict.Add(Power,Reduce 7)
            dict.Add(Lpar,e_mis_lpar)
            dict.Add(Rpar,Reduce 7)
            dict.Add(Comma,Reduce 7)
            dict.Add(Dollar,Reduce 7)
            dict
            )(new Dictionary<Token,Action>())
        //s2
        (fun (dict : Dictionary<Token,Action>) ->
            dict.Add(Int "",e_op_exp)
            dict.Add(Float "",e_op_exp)
            dict.Add(Id "",e_op_exp)
            dict.Add(Plus,Reduce 6)
            dict.Add(Minus,Reduce 6)
            dict.Add(Times,Reduce 6)
            dict.Add(Divide,Reduce 6)
            dict.Add(Power,Reduce 6)
            dict.Add(Lpar,e_mis_lpar)
            dict.Add(Rpar,Reduce 6)
            dict.Add(Comma,Reduce 6)
            dict.Add(Dollar,Reduce 6)
            dict
            )(new Dictionary<Token,Action>())
        //s3
        (fun (dict : Dictionary<Token,Action>) ->
            dict.Add(Int "",e_op_exp)
            dict.Add(Float "",e_op_exp)
            dict.Add(Id "",e_op_exp)
            dict.Add(Plus,Shift 11)
            dict.Add(Minus,Shift 10)
            dict.Add(Times,Shift 13)
            dict.Add(Divide,Shift 9)
            dict.Add(Power,Shift 12)
            dict.Add(Lpar,e_mis_lpar)
            dict.Add(Rpar,e_mis_rpar)
            dict.Add(Comma,e_op_exp)
            dict.Add(Dollar,Accept)
            dict
            )(new Dictionary<Token,Action>())
        //s4
        (fun (dict : Dictionary<Token,Action>) ->
            dict.Add(Int "",e_op_exp)
            dict.Add(Float "",e_op_exp)
            dict.Add(Id "",e_op_exp)
            dict.Add(Plus,Reduce 15)
            dict.Add(Minus,Reduce 15)
            dict.Add(Times,Reduce 15)
            dict.Add(Divide,Reduce 15)
            dict.Add(Power,Reduce 15)
            dict.Add(Lpar,e_mis_lpar)
            dict.Add(Rpar,Reduce 15)
            dict.Add(Comma,Reduce 15)
            dict.Add(Dollar,Reduce 15)
            dict
            )(new Dictionary<Token,Action>())
        //s5
        (fun (dict : Dictionary<Token,Action>) ->
            dict.Add(Int "",e_args_any)
            dict.Add(Float "",e_args_any)
            dict.Add(Id "",e_args_any)
            dict.Add(Plus,e_args_any)
            dict.Add(Minus,e_args_any)
            dict.Add(Times,e_args_any)
            dict.Add(Divide,e_args_any)
            dict.Add(Power,e_args_any)
            dict.Add(Lpar,Shift 14)
            dict.Add(Rpar,e_args_any)
            dict.Add(Comma,e_args_any)
            dict.Add(Dollar,e_args_any)
            dict
            )(new Dictionary<Token,Action>())
        //s6
        (fun (dict : Dictionary<Token,Action>) ->
            dict.Add(Int "",e_op_exp)
            dict.Add(Float "",e_op_exp)
            dict.Add(Id "",e_op_exp)
            dict.Add(Plus,Reduce 16)
            dict.Add(Minus,Reduce 16)
            dict.Add(Times,Reduce 16)
            dict.Add(Divide,Reduce 16)
            dict.Add(Power,Reduce 16)
            dict.Add(Lpar,e_mis_lpar)
            dict.Add(Rpar,Reduce 16)
            dict.Add(Comma,Reduce 16)
            dict.Add(Dollar,Reduce 16)
            dict
            )(new Dictionary<Token,Action>())
        //s7
        (fun (dict : Dictionary<Token,Action>) ->
            dict.Add(Int "",Shift 6)
            dict.Add(Float "",Shift 4)
            dict.Add(Id "",Shift 5)
            dict.Add(Plus,e_exp_op)
            dict.Add(Minus,Shift 8)
            dict.Add(Times,e_exp_op)
            dict.Add(Divide,e_exp_op)
            dict.Add(Power,e_exp_op)
            dict.Add(Lpar,Shift 7)
            dict.Add(Rpar,e_mis_rpar)
            dict.Add(Comma,e_mis_comma)
            dict.Add(Dollar,e_exp_eoi)
            dict
            )(new Dictionary<Token,Action>())
        //s8
        (fun (dict : Dictionary<Token,Action>) ->
            dict.Add(Int "",Shift 6)
            dict.Add(Float "",Shift 4)
            dict.Add(Id "",Shift 5)
            dict.Add(Plus,e_exp_op)
            dict.Add(Minus,Shift 8)
            dict.Add(Times,e_exp_op)
            dict.Add(Divide,e_exp_op)
            dict.Add(Power,e_exp_op)
            dict.Add(Lpar,Shift 7)
            dict.Add(Rpar,e_mis_rpar)
            dict.Add(Comma,e_mis_comma)
            dict.Add(Dollar,e_exp_eoi)
            dict
            )(new Dictionary<Token,Action>())
        //s9
        (fun (dict : Dictionary<Token,Action>) ->
            dict.Add(Int "",Shift 6)
            dict.Add(Float "",Shift 4)
            dict.Add(Id "",Shift 5)
            dict.Add(Plus,e_exp_op)
            dict.Add(Minus,Shift 8)
            dict.Add(Times,e_exp_op)
            dict.Add(Divide,e_exp_op)
            dict.Add(Power,e_exp_op)
            dict.Add(Lpar,Shift 7)
            dict.Add(Rpar,e_mis_rpar)
            dict.Add(Comma,e_mis_comma)
            dict.Add(Dollar,e_exp_eoi)
            dict
            )(new Dictionary<Token,Action>())
        //s10
        (fun (dict : Dictionary<Token,Action>) ->
            dict.Add(Int "",Shift 6)
            dict.Add(Float "",Shift 4)
            dict.Add(Id "",Shift 5)
            dict.Add(Plus,e_exp_op)
            dict.Add(Minus,Shift 8)
            dict.Add(Times,e_exp_op)
            dict.Add(Divide,e_exp_op)
            dict.Add(Power,e_exp_op)
            dict.Add(Lpar,Shift 7)
            dict.Add(Rpar,e_mis_rpar)
            dict.Add(Comma,e_mis_comma)
            dict.Add(Dollar,e_exp_eoi)
            dict
            )(new Dictionary<Token,Action>())
        //s11
        (fun (dict : Dictionary<Token,Action>) ->
            dict.Add(Int "",Shift 6)
            dict.Add(Float "",Shift 4)
            dict.Add(Id "",Shift 5)
            dict.Add(Plus,e_exp_op)
            dict.Add(Minus,Shift 8)
            dict.Add(Times,e_exp_op)
            dict.Add(Divide,e_exp_op)
            dict.Add(Power,e_exp_op)
            dict.Add(Lpar,Shift 7)
            dict.Add(Rpar,e_mis_rpar)
            dict.Add(Comma,e_mis_comma)
            dict.Add(Dollar,e_exp_eoi)
            dict
            )(new Dictionary<Token,Action>())
        //s12
        (fun (dict : Dictionary<Token,Action>) ->
            dict.Add(Int "",Shift 6)
            dict.Add(Float "",Shift 4)
            dict.Add(Id "",Shift 5)
            dict.Add(Plus,e_exp_op)
            dict.Add(Minus,Shift 8)
            dict.Add(Times,e_exp_op)
            dict.Add(Divide,e_exp_op)
            dict.Add(Power,e_exp_op)
            dict.Add(Lpar,Shift 7)
            dict.Add(Rpar,e_mis_rpar)
            dict.Add(Comma,e_mis_comma)
            dict.Add(Dollar,e_exp_eoi)
            dict
            )(new Dictionary<Token,Action>())
        //s13
        (fun (dict : Dictionary<Token,Action>) ->
            dict.Add(Int "",Shift 6)
            dict.Add(Float "",Shift 4)
            dict.Add(Id "",Shift 5)
            dict.Add(Plus,e_exp_op)
            dict.Add(Minus,Shift 8)
            dict.Add(Times,e_exp_op)
            dict.Add(Divide,e_exp_op)
            dict.Add(Power,e_exp_op)
            dict.Add(Lpar,Shift 7)
            dict.Add(Rpar,e_mis_rpar)
            dict.Add(Comma,e_mis_comma)
            dict.Add(Dollar,e_exp_eoi)
            dict
            )(new Dictionary<Token,Action>())
        //s14
        (fun (dict : Dictionary<Token,Action>) ->
            dict.Add(Int "",Shift 6)
            dict.Add(Float "",Shift 4)
            dict.Add(Id "",Shift 5)
            dict.Add(Plus,e_exp_op)
            dict.Add(Minus,Shift 8)
            dict.Add(Times,e_exp_op)
            dict.Add(Divide,e_exp_op)
            dict.Add(Power,e_exp_op)
            dict.Add(Lpar,Shift 7)
            dict.Add(Rpar,e_mis_rpar)
            dict.Add(Comma,e_mis_comma)
            dict.Add(Dollar,e_exp_eoi)
            dict
            )(new Dictionary<Token,Action>())
        //s15
        (fun (dict : Dictionary<Token,Action>) ->
            dict.Add(Int "",e_op_exp)
            dict.Add(Float "",e_op_exp)
            dict.Add(Id "",e_op_exp)
            dict.Add(Plus,Shift 11)
            dict.Add(Minus,Shift 10)
            dict.Add(Times,Shift 13)
            dict.Add(Divide,Shift 9)
            dict.Add(Power,Shift 12)
            dict.Add(Lpar,e_mis_lpar)
            dict.Add(Rpar,Shift 26)
            dict.Add(Comma,e_mis_comma)
            dict.Add(Dollar,e_op_eoi)
            dict
            )(new Dictionary<Token,Action>())
        //s16
        (fun (dict : Dictionary<Token,Action>) ->
            dict.Add(Int "",e_op_exp)
            dict.Add(Float "",e_op_exp)
            dict.Add(Id "",e_op_exp)
            dict.Add(Plus,Reduce 8)
            dict.Add(Minus,Reduce 8)
            dict.Add(Times,Shift 13)
            dict.Add(Divide,Shift 9)
            dict.Add(Power,Shift 12)
            dict.Add(Lpar,e_mis_lpar)
            dict.Add(Rpar,Reduce 8)
            dict.Add(Comma,Reduce 8)
            dict.Add(Dollar,Reduce 8)
            dict
            )(new Dictionary<Token,Action>())
        //s17
        (fun (dict : Dictionary<Token,Action>) ->
            dict.Add(Int "",e_op_exp)
            dict.Add(Float "",e_op_exp)
            dict.Add(Id "",e_op_exp)
            dict.Add(Plus,Reduce 4)
            dict.Add(Minus,Reduce 4)
            dict.Add(Times,Reduce 4)
            dict.Add(Divide,Reduce 4)
            dict.Add(Power,Shift 12)
            dict.Add(Lpar,e_mis_lpar)
            dict.Add(Rpar,Reduce 4)
            dict.Add(Comma,Reduce 4)
            dict.Add(Dollar,Reduce 4)
            dict
            )(new Dictionary<Token,Action>())
        //s18
        (fun (dict : Dictionary<Token,Action>) ->
            dict.Add(Int "",e_op_exp)
            dict.Add(Float "",e_op_exp)
            dict.Add(Id "",e_op_exp)
            dict.Add(Plus,Reduce 2)
            dict.Add(Minus,Reduce 2)
            dict.Add(Times,Shift 13)
            dict.Add(Divide,Shift 9)
            dict.Add(Power,Shift 12)
            dict.Add(Lpar,e_mis_lpar)
            dict.Add(Rpar,Reduce 2)
            dict.Add(Comma,Reduce 2)
            dict.Add(Dollar,Reduce 2)
            dict
            )(new Dictionary<Token,Action>())
        //s19
        (fun (dict : Dictionary<Token,Action>) ->
            dict.Add(Int "",e_op_exp)
            dict.Add(Float "",e_op_exp)
            dict.Add(Id "",e_op_exp)
            dict.Add(Plus,Reduce 1)
            dict.Add(Minus,Reduce 1)
            dict.Add(Times,Shift 13)
            dict.Add(Divide,Shift 9)
            dict.Add(Power,Shift 12)
            dict.Add(Lpar,e_mis_lpar)
            dict.Add(Rpar,Reduce 1)
            dict.Add(Comma,Reduce 1)
            dict.Add(Dollar,Reduce 1)
            dict
            )(new Dictionary<Token,Action>())
        //s20
        (fun (dict : Dictionary<Token,Action>) ->
            dict.Add(Int "",e_op_exp)
            dict.Add(Float "",e_op_exp)
            dict.Add(Id "",e_op_exp)
            dict.Add(Plus,Reduce 5)
            dict.Add(Minus,Reduce 5)
            dict.Add(Times,Reduce 5)
            dict.Add(Divide,Reduce 5)
            dict.Add(Power,Shift 12)
            dict.Add(Lpar,e_mis_lpar)
            dict.Add(Rpar,Reduce 5)
            dict.Add(Comma,Reduce 5)
            dict.Add(Dollar,Reduce 5)
            dict
            )(new Dictionary<Token,Action>())
        //s21
        (fun (dict : Dictionary<Token,Action>) ->
            dict.Add(Int "",e_op_exp)
            dict.Add(Float "",e_op_exp)
            dict.Add(Id "",e_op_exp)
            dict.Add(Plus,Reduce 3)
            dict.Add(Minus,Reduce 3)
            dict.Add(Times,Reduce 3)
            dict.Add(Divide,Reduce 3)
            dict.Add(Power,Shift 12)
            dict.Add(Lpar,e_mis_lpar)
            dict.Add(Rpar,Reduce 3)
            dict.Add(Comma,Reduce 3)
            dict.Add(Dollar,Reduce 3)
            dict
            )(new Dictionary<Token,Action>())
        //s22
        (fun (dict : Dictionary<Token,Action>) ->
            dict.Add(Int "",e_args_end)
            dict.Add(Float "",e_args_end)
            dict.Add(Id "",e_args_end)
            dict.Add(Plus,e_args_end)
            dict.Add(Minus,e_args_end)
            dict.Add(Times,e_args_end)
            dict.Add(Divide,e_args_end)
            dict.Add(Power,e_args_end)
            dict.Add(Lpar,e_args_end)
            dict.Add(Rpar,Reduce 10)
            dict.Add(Comma,Shift 27)
            dict.Add(Dollar,e_args_end)
            dict
            )(new Dictionary<Token,Action>())
        //s23
        (fun (dict : Dictionary<Token,Action>) ->
            dict.Add(Int "",e_args_end)
            dict.Add(Float "",e_args_end)
            dict.Add(Id "",e_args_end)
            dict.Add(Plus,e_args_end)
            dict.Add(Minus,e_args_end)
            dict.Add(Times,e_args_end)
            dict.Add(Divide,e_args_end)
            dict.Add(Power,e_args_end)
            dict.Add(Lpar,e_args_end)
            dict.Add(Rpar,Shift 28)
            dict.Add(Comma,e_args_end)
            dict.Add(Dollar,e_args_end)
            dict
            )(new Dictionary<Token,Action>())
        //s24
        (fun (dict : Dictionary<Token,Action>) ->
            dict.Add(Int "",e_op_exp)
            dict.Add(Float "",e_op_exp)
            dict.Add(Id "",e_op_exp)
            dict.Add(Plus,Reduce 6)
            dict.Add(Minus,Reduce 6)
            dict.Add(Times,Reduce 6)
            dict.Add(Divide,Reduce 6)
            dict.Add(Power,Reduce 6)
            dict.Add(Lpar,e_mis_lpar)
            dict.Add(Rpar,Reduce 6)
            dict.Add(Comma,Reduce 6)
            dict.Add(Dollar,Reduce 6)
            dict
            )(new Dictionary<Token,Action>())
        //s25
        (fun (dict : Dictionary<Token,Action>) ->
            dict.Add(Int "",e_op_exp)
            dict.Add(Float "",e_op_exp)
            dict.Add(Id "",e_op_exp)
            dict.Add(Plus,Shift 11)
            dict.Add(Minus,Shift 10)
            dict.Add(Times,Shift 13)
            dict.Add(Divide,Shift 9)
            dict.Add(Power,Shift 12)
            dict.Add(Lpar,e_mis_lpar)
            dict.Add(Rpar,Reduce 12)
            dict.Add(Comma,Reduce 12)
            dict.Add(Dollar,e_op_exp)
            dict
            )(new Dictionary<Token,Action>())
        //s26
        (fun (dict : Dictionary<Token,Action>) ->
            dict.Add(Int "",e_op_exp)
            dict.Add(Float "",e_op_exp)
            dict.Add(Id "",e_op_exp)
            dict.Add(Plus,Reduce 14)
            dict.Add(Minus,Reduce 14)
            dict.Add(Times,Reduce 14)
            dict.Add(Divide,Reduce 14)
            dict.Add(Power,Reduce 14)
            dict.Add(Lpar,e_mis_lpar)
            dict.Add(Rpar,Reduce 14)
            dict.Add(Comma,Reduce 14)
            dict.Add(Dollar,Reduce 14)
            dict
            )(new Dictionary<Token,Action>())
        //s27
        (fun (dict : Dictionary<Token,Action>) ->
            dict.Add(Int "",Shift 6)
            dict.Add(Float "",Shift 4)
            dict.Add(Id "",Shift 5)
            dict.Add(Plus,e_exp_op)
            dict.Add(Minus,Shift 8)
            dict.Add(Times,e_exp_op)
            dict.Add(Divide,e_exp_op)
            dict.Add(Power,e_exp_op)
            dict.Add(Lpar,Shift 7)
            dict.Add(Rpar,e_mis_rpar)
            dict.Add(Comma,e_mis_comma)
            dict.Add(Dollar,e_exp_eoi)
            dict
            )(new Dictionary<Token,Action>())
        //s28
        (fun (dict : Dictionary<Token,Action>) ->
            dict.Add(Int "",e_op_exp)
            dict.Add(Float "",e_op_exp)
            dict.Add(Id "",e_op_exp)
            dict.Add(Plus,Reduce 9)
            dict.Add(Minus,Reduce 9)
            dict.Add(Times,Reduce 9)
            dict.Add(Divide,Reduce 9)
            dict.Add(Power,Reduce 9)
            dict.Add(Lpar,e_mis_lpar)
            dict.Add(Rpar,Reduce 9)
            dict.Add(Comma,Reduce 9)
            dict.Add(Dollar,Reduce 9)
            dict
            )(new Dictionary<Token,Action>())
        //s29
        (fun (dict : Dictionary<Token,Action>) ->
            dict.Add(Int "",e_args_end)
            dict.Add(Float "",e_args_end)
            dict.Add(Id "",e_args_end)
            dict.Add(Plus,e_args_end)
            dict.Add(Minus,e_args_end)
            dict.Add(Times,e_args_end)
            dict.Add(Divide,e_args_end)
            dict.Add(Power,e_args_end)
            dict.Add(Lpar,e_args_end)
            dict.Add(Rpar,Reduce 11)
            dict.Add(Comma,e_args_end)
            dict.Add(Dollar,e_args_end)
            dict
            )(new Dictionary<Token,Action>())
        |]
    let gotoTable = [|
        (fun (dict : Dictionary<string,Option<int>>) ->
            dict.Add("Exp",Some 3)
            dict.Add("Call",Some 2)
            dict.Add("Args",None)
            dict.Add("Arg",None)
            dict.Add("Atom",Some 1)
            dict
            )(new Dictionary<string,Option<int>>())
        (fun (dict : Dictionary<string,Option<int>>) ->
            dict.Add("Exp",None)
            dict.Add("Call",None)
            dict.Add("Args",None)
            dict.Add("Arg",None)
            dict.Add("Atom",None)
            dict
            )(new Dictionary<string,Option<int>>())
        (fun (dict : Dictionary<string,Option<int>>) ->
            dict.Add("Exp",None)
            dict.Add("Call",None)
            dict.Add("Args",None)
            dict.Add("Arg",None)
            dict.Add("Atom",None)
            dict
            )(new Dictionary<string,Option<int>>())
        (fun (dict : Dictionary<string,Option<int>>) ->
            dict.Add("Exp",None)
            dict.Add("Call",None)
            dict.Add("Args",None)
            dict.Add("Arg",None)
            dict.Add("Atom",None)
            dict
            )(new Dictionary<string,Option<int>>())
        (fun (dict : Dictionary<string,Option<int>>) ->
            dict.Add("Exp",None)
            dict.Add("Call",None)
            dict.Add("Args",None)
            dict.Add("Arg",None)
            dict.Add("Atom",None)
            dict
            )(new Dictionary<string,Option<int>>())
        (fun (dict : Dictionary<string,Option<int>>) ->
            dict.Add("Exp",None)
            dict.Add("Call",None)
            dict.Add("Args",None)
            dict.Add("Arg",None)
            dict.Add("Atom",None)
            dict
            )(new Dictionary<string,Option<int>>())
        (fun (dict : Dictionary<string,Option<int>>) ->
            dict.Add("Exp",None)
            dict.Add("Call",None)
            dict.Add("Args",None)
            dict.Add("Arg",None)
            dict.Add("Atom",None)
            dict
            )(new Dictionary<string,Option<int>>())
        (fun (dict : Dictionary<string,Option<int>>) ->
            dict.Add("Exp",Some 15)
            dict.Add("Call",Some 2)
            dict.Add("Args",None)
            dict.Add("Arg",None)
            dict.Add("Atom",Some 1)
            dict
            )(new Dictionary<string,Option<int>>())
        (fun (dict : Dictionary<string,Option<int>>) ->
            dict.Add("Exp",Some 16)
            dict.Add("Call",Some 2)
            dict.Add("Args",None)
            dict.Add("Arg",None)
            dict.Add("Atom",Some 1)
            dict
            )(new Dictionary<string,Option<int>>())
        (fun (dict : Dictionary<string,Option<int>>) ->
            dict.Add("Exp",Some 17)
            dict.Add("Call",Some 2)
            dict.Add("Args",None)
            dict.Add("Arg",None)
            dict.Add("Atom",Some 1)
            dict
            )(new Dictionary<string,Option<int>>())
        (fun (dict : Dictionary<string,Option<int>>) ->
            dict.Add("Exp",Some 18)
            dict.Add("Call",Some 2)
            dict.Add("Args",None)
            dict.Add("Arg",None)
            dict.Add("Atom",Some 1)
            dict
            )(new Dictionary<string,Option<int>>())
        (fun (dict : Dictionary<string,Option<int>>) ->
            dict.Add("Exp",Some 19)
            dict.Add("Call",Some 2)
            dict.Add("Args",None)
            dict.Add("Arg",None)
            dict.Add("Atom",Some 1)
            dict
            )(new Dictionary<string,Option<int>>())
        (fun (dict : Dictionary<string,Option<int>>) ->
            dict.Add("Exp",Some 20)
            dict.Add("Call",Some 2)
            dict.Add("Args",None)
            dict.Add("Arg",None)
            dict.Add("Atom",Some 1)
            dict
            )(new Dictionary<string,Option<int>>())
        (fun (dict : Dictionary<string,Option<int>>) ->
            dict.Add("Exp",Some 21)
            dict.Add("Call",Some 2)
            dict.Add("Args",None)
            dict.Add("Arg",None)
            dict.Add("Atom",Some 1)
            dict
            )(new Dictionary<string,Option<int>>())
        (fun (dict : Dictionary<string,Option<int>>) ->
            dict.Add("Exp",Some 25)
            dict.Add("Call",Some 24)
            dict.Add("Args",Some 23)
            dict.Add("Arg",Some 22)
            dict.Add("Atom",Some 1)
            dict
            )(new Dictionary<string,Option<int>>())
        (fun (dict : Dictionary<string,Option<int>>) ->
            dict.Add("Exp",None)
            dict.Add("Call",None)
            dict.Add("Args",None)
            dict.Add("Arg",None)
            dict.Add("Atom",None)
            dict
            )(new Dictionary<string,Option<int>>())
        (fun (dict : Dictionary<string,Option<int>>) ->
            dict.Add("Exp",None)
            dict.Add("Call",None)
            dict.Add("Args",None)
            dict.Add("Arg",None)
            dict.Add("Atom",None)
            dict
            )(new Dictionary<string,Option<int>>())
        (fun (dict : Dictionary<string,Option<int>>) ->
            dict.Add("Exp",None)
            dict.Add("Call",None)
            dict.Add("Args",None)
            dict.Add("Arg",None)
            dict.Add("Atom",None)
            dict
            )(new Dictionary<string,Option<int>>())
        (fun (dict : Dictionary<string,Option<int>>) ->
            dict.Add("Exp",None)
            dict.Add("Call",None)
            dict.Add("Args",None)
            dict.Add("Arg",None)
            dict.Add("Atom",None)
            dict
            )(new Dictionary<string,Option<int>>())
        (fun (dict : Dictionary<string,Option<int>>) ->
            dict.Add("Exp",None)
            dict.Add("Call",None)
            dict.Add("Args",None)
            dict.Add("Arg",None)
            dict.Add("Atom",None)
            dict
            )(new Dictionary<string,Option<int>>())
        (fun (dict : Dictionary<string,Option<int>>) ->
            dict.Add("Exp",None)
            dict.Add("Call",None)
            dict.Add("Args",None)
            dict.Add("Arg",None)
            dict.Add("Atom",None)
            dict
            )(new Dictionary<string,Option<int>>())
        (fun (dict : Dictionary<string,Option<int>>) ->
            dict.Add("Exp",None)
            dict.Add("Call",None)
            dict.Add("Args",None)
            dict.Add("Arg",None)
            dict.Add("Atom",None)
            dict
            )(new Dictionary<string,Option<int>>())
        (fun (dict : Dictionary<string,Option<int>>) ->
            dict.Add("Exp",None)
            dict.Add("Call",None)
            dict.Add("Args",None)
            dict.Add("Arg",None)
            dict.Add("Atom",None)
            dict
            )(new Dictionary<string,Option<int>>())
        (fun (dict : Dictionary<string,Option<int>>) ->
            dict.Add("Exp",None)
            dict.Add("Call",None)
            dict.Add("Args",None)
            dict.Add("Arg",None)
            dict.Add("Atom",None)
            dict
            )(new Dictionary<string,Option<int>>())
        (fun (dict : Dictionary<string,Option<int>>) ->
            dict.Add("Exp",None)
            dict.Add("Call",None)
            dict.Add("Args",None)
            dict.Add("Arg",None)
            dict.Add("Atom",None)
            dict
            )(new Dictionary<string,Option<int>>())
        (fun (dict : Dictionary<string,Option<int>>) ->
            dict.Add("Exp",None)
            dict.Add("Call",None)
            dict.Add("Args",None)
            dict.Add("Arg",None)
            dict.Add("Atom",None)
            dict
            )(new Dictionary<string,Option<int>>())
        (fun (dict : Dictionary<string,Option<int>>) ->
            dict.Add("Exp",None)
            dict.Add("Call",None)
            dict.Add("Args",None)
            dict.Add("Arg",None)
            dict.Add("Atom",None)
            dict
            )(new Dictionary<string,Option<int>>())
        (fun (dict : Dictionary<string,Option<int>>) ->
            dict.Add("Exp",Some 25)
            dict.Add("Call",Some 24)
            dict.Add("Args",Some 29)
            dict.Add("Arg",Some 22)
            dict.Add("Atom",Some 1)
            dict
            )(new Dictionary<string,Option<int>>())
        (fun (dict : Dictionary<string,Option<int>>) ->
            dict.Add("Exp",None)
            dict.Add("Call",None)
            dict.Add("Args",None)
            dict.Add("Arg",None)
            dict.Add("Atom",None)
            dict
            )(new Dictionary<string,Option<int>>())
        (fun (dict : Dictionary<string,Option<int>>) ->
            dict.Add("Exp",None)
            dict.Add("Call",None)
            dict.Add("Args",None)
            dict.Add("Arg",None)
            dict.Add("Atom",None)
            dict
            )(new Dictionary<string,Option<int>>())
        |]
    let productions_str = [|
        ("Exp'",[|"Exp"|]) //0
        ("Exp",[|"Exp";"plus";"Exp"|]) //1
        ("Exp",[|"Exp";"minus";"Exp"|]) //2
        ("Exp",[|"Exp";"times";"Exp"|]) //3
        ("Exp",[|"Exp";"divide";"Exp"|]) //4
        ("Exp",[|"Exp";"power";"Exp"|]) //5
        ("Exp",[|"Call"|]) //6
        ("Exp",[|"Atom"|]) //7
        ("Exp",[|"minus";"Exp"|]) //8
        ("Call",[|"id";"lpar";"Args";"rpar"|]) //9
        ("Args",[|"Arg"|]) //10
        ("Args",[|"Arg";"comma";"Args"|]) //11
        ("Arg",[|"Exp"|]) //12
        ("Arg",[|"Call"|]) //13
        ("Atom",[|"lpar";"Exp";"rpar"|]) //14
        ("Atom",[|"float"|]) //15
        ("Atom",[|"int"|]) //16
        |]
    let args2arr tree =
        let argCount =
            let rec count acc = function
                | (ArgResult _)::tree -> count (acc + 1) tree
                | _ -> acc
            count 0 tree
        let args =
            Array.init
                argCount
                (fun i ->
                    let i0 = argCount - i - 1
                    match tree.[i0] with
                    | (ArgResult r) -> r
                    | _ -> failwith "internal error"    
                    )
        (args,tree.[argCount ..])
    let productions_fun = [|
        //[0]Exp' -> Exp
        (fun tree ftab -> tree)
        //[1]Exp -> Exp plus Exp
        (fun tree ftab ->
            match tree with
            | (Result r)::(Result l)::tree ->
                (Result (l + r))::tree
            | _ -> tree
            )
        //[2]Exp -> Exp minus Exp
        (fun tree ftab ->
            match tree with
            | (Result r)::(Result l)::tree ->
                (Result (l - r))::tree
            | _ -> tree 
            )
        //[3]Exp -> Exp times Exp
        (fun tree ftab ->
            match tree with
            | (Result r)::(Result l)::tree ->
                (Result (l * r))::tree
            | _ -> tree
            )
        //[4]Exp -> Exp divide Exp
        (fun tree ftab ->
            match tree with
            | (Result r)::(Result l)::tree ->
                (Result (l / r))::tree
            | _ -> tree
            )
        //[5]Exp -> Exp power Exp
        (fun tree ftab ->
            match tree with
            | (Result r)::(Result l)::tree ->
                (Result (System.Math.Pow(l,r)))::tree
            | _ -> tree
            )
        //[6]Exp -> Call
        (fun tree ftab -> tree)
        //[7]Exp -> Atom
        (fun tree ftab -> tree)
        //[8]Exp -> minus Exp
        (fun tree ftab ->
            match tree with
            | (Result r)::tree ->
                (Result (-r))::tree
            | _ -> tree
            )
        //[9]Call -> id lpar Args rpar
        (fun tree (ftab : Dictionary<string,int[] * System.Func<double[],double>>) ->
            let (args,newTree) = args2arr tree
            match newTree with
            | (FuncId fid)::tree when ftab.ContainsKey(fid) ->
                let (argNumberA,f) = ftab.[fid]
                let argsGiven = Array.length args
                if Array.exists (fun x -> x = argsGiven) argNumberA then
                    try (Result (f.Invoke(args)))::tree with
                    | _ -> failwith (sprintf "overflow in function %s" fid)
                else
                    failwith (sprintf "not the right amount of args given to %s" fid)
            | (FuncId fid)::tree ->
                failwith "function does not exist"
            | _ -> tree
            )
        //[10]Args -> Arg
        (fun tree ftab -> tree)
        //[11]Args -> Arg comma Args
        (fun tree ftab -> tree)
        //[12]Arg -> Exp
        (fun tree ftab ->
            match tree with
            | (Result r)::tree ->
                (ArgResult r)::tree
            | _ -> tree
            )
        //[13]Arg -> Call
        (fun tree ftab ->
            match tree with
            | (Result r)::tree ->
                (ArgResult r)::tree
            | _ -> tree
            )
        //[14]Atom -> lpar Exp rpar
        (fun tree ftab -> tree)
        //[15]Atom -> float
        (fun tree ftab ->
            match tree with
            | (ConstResult c)::tree ->
                (Result c)::tree
            | _ -> tree
            )
        //[16]Atom -> int
        (fun tree ftab ->
            match tree with
            | (ConstResult c)::tree ->
                (Result c)::tree
            | _ -> tree
            )
        |]
    let parser tokens ftab =
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
            if i < tLen then
                (i + 1,tokens.[i+1])
            else failwith "parser: getNextFromInputError"
        let removeAtomVal = function
            | Int _ -> Int ""
            | Float _ -> Float ""
            | Id _ -> Id ""
            | t -> t
        let addAtom2tree tree node =
            match node with
            | Float n | Int n -> (ConstResult (float n))::tree
            | Id id -> (FuncId id)::tree
            | _ -> tree
        let rec exec (i,a) sStack tree =
            let (s,_) = popOne sStack
            match actionTable.[s].[removeAtomVal a] with
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
                let newTree = prodF tree ftab
                let betaLen = Array.length rSide
                let (_,newStack) = popN betaLen sStack
                let (t,_) = popOne newStack
                let newStack = pushGoto newStack gotoTable.[t].[prod]
                exec (i,a) newStack newTree
            | Accept -> tree
            | Error msg ->
                failwith (sprintf "syntax error: %s" msg)
        exec (0,tokens.[0]) [0] []
    let parse str (ftab : Dictionary<string,int[] * System.Func<float[],float>>) =
        let lexed = lexer str
        let parsed = parser lexed ftab
        parsed
