module ComCalcParser =
    open System.Collections.Generic
    open System.Text.RegularExpressions
    type Action =
        | Shift of int
        | Reduce of int
        | Accept
        | Error of string
    type Tree =
        | EmptyTree
    (* Initialize tree stack.
     * This is passed on to the production_funs as tree
     * *)
    let initTreeStack = [EmptyTree]
    (* Add leaf nodes/tokens here.
     * That is nodes of type token
     * that is to be leafs in the tree
     * *)
    let addLeaf2tree tree node =
        match node with
        | (name,Some v) ->
            printfn "leave : %s" v
            tree
         | (name,None) ->
            tree
    (* Add handling of productions here.
     * That is insert into tree or outside
     * data-structures.
     * *)
    let productions_fun = [|
        //[0] __ -> Exp' 
        (fun tree -> [EmptyTree])
        //[1] Exp' -> Exp 
        (fun tree -> [EmptyTree])
        //[2] Exp -> Exp minus Exp 
        (fun tree -> [EmptyTree])
        //[3] Exp -> Exp plus Exp 
        (fun tree -> [EmptyTree])
        //[4] Exp -> Exp times Exp 
        (fun tree -> [EmptyTree])
        //[5] Exp -> Exp divide Exp 
        (fun tree -> [EmptyTree])
        //[6] Exp -> Exp power Exp 
        (fun tree -> [EmptyTree])
        //[7] Exp -> Call 
        (fun tree -> [EmptyTree])
        //[8] Exp -> Atom 
        (fun tree -> [EmptyTree])
        //[9] Exp -> minus Exp 
        (fun tree -> [EmptyTree])
        //[10] Call -> id lpar Args rpar 
        (fun tree -> [EmptyTree])
        //[11] Args -> Arg 
        (fun tree -> [EmptyTree])
        //[12] Args -> Arg comma Args 
        (fun tree -> [EmptyTree])
        //[13] Arg -> Atom 
        (fun tree -> [EmptyTree])
        //[14] Arg -> Call 
        (fun tree -> [EmptyTree])
        //[15] Arg -> 
        (fun tree -> [EmptyTree])
        //[16] Atom -> lpar Exp rpar 
        (fun tree -> [EmptyTree])
        //[17] Atom -> float 
        (fun tree -> [EmptyTree])
        //[18] Atom -> int 
        (fun tree -> [EmptyTree])
        |]
    let lexer inStr =
        let tokensL = new List<string * Option<string>>()
        let addToken (tGroup : GroupCollection) =
            if tGroup.[1].Value <> "" then
                tokensL.Add(("comma",None))
            if tGroup.[2].Value <> "" then
                tokensL.Add(("divide",None))
            if tGroup.[3].Value <> "" then
                tokensL.Add(("float",Some tGroup.[3].Value))
            if tGroup.[4].Value <> "" then
                tokensL.Add(("id",Some tGroup.[4].Value))
            if tGroup.[5].Value <> "" then
                tokensL.Add(("int",Some tGroup.[5].Value))
            if tGroup.[6].Value <> "" then
                tokensL.Add(("lpar",None))
            if tGroup.[7].Value <> "" then
                tokensL.Add(("minus",None))
            if tGroup.[8].Value <> "" then
                tokensL.Add(("plus",None))
            if tGroup.[9].Value <> "" then
                tokensL.Add(("power",None))
            if tGroup.[10].Value <> "" then
                tokensL.Add(("rpar",None))
            if tGroup.[11].Value <> "" then
                tokensL.Add(("times",None))
        let regToken =
                "(,)|"+
                "(/)|"+
                "([0-9]*.[0-9]+)|"+
                "([a-zA-Z]+)|"+
                "([0-9]+)|"+
                "(()|"+
                "(-)|"+
                "(\\+)|"+
                "(\\^)|"+
                "())|"+
                "(\\*)|"+
                "\n|"+
                "\r|"+
                "\t|"+
                " |"+
                "$"
        let mfun (m : Match) =
            addToken m.Groups
            ""
        let residueStr = Regex.Replace(inStr,regToken,mfun)
        if residueStr <> "" then
            failwith (sprintf "garbage in expression: %c" residueStr.[0])
        else
            tokensL.Add("$",None)
            Array.init tokensL.Count (fun i -> tokensL.[i])
    let actionTable = [|
        //s0
        (fun (dict : Dictionary<string,Action>) ->
            dict.Add("comma",Error "expected 'operand','parentheses','operator', but given 'comma'")
            dict.Add("divide",Error "expected 'operand','parentheses','operator', but given 'operator'")
            dict.Add("float",Shift 5)
            dict.Add("id",Shift 6)
            dict.Add("int",Shift 7)
            dict.Add("lpar",Shift 8)
            dict.Add("minus",Shift 9)
            dict.Add("plus",Error "expected 'operand','parentheses','operator', but given 'operator'")
            dict.Add("power",Error "expected 'operand','parentheses','operator', but given 'power'")
            dict.Add("rpar",Error "expected 'operand','parentheses','operator', but given 'parentheses'")
            dict.Add("times",Error "expected 'operand','parentheses','operator', but given 'times'")
            dict.Add("$",Error "expected 'operand','parentheses','operator', but given '$'")
            dict
            )(new Dictionary<string,Action>())
        //s1
        (fun (dict : Dictionary<string,Action>) ->
            dict.Add("comma",Error "expected 'operator','power','parentheses','times','eoi', but given 'comma'")
            dict.Add("divide",Reduce 8)
            dict.Add("float",Error "expected 'operator','power','parentheses','times','eoi', but given 'operand'")
            dict.Add("id",Error "expected 'operator','power','parentheses','times','eoi', but given 'operand'")
            dict.Add("int",Error "expected 'operator','power','parentheses','times','eoi', but given 'operand'")
            dict.Add("lpar",Error "expected 'operator','power','parentheses','times','eoi', but given 'parentheses'")
            dict.Add("minus",Reduce 8)
            dict.Add("plus",Reduce 8)
            dict.Add("power",Reduce 8)
            dict.Add("rpar",Reduce 8)
            dict.Add("times",Reduce 8)
            dict.Add("$",Reduce 8)
            dict
            )(new Dictionary<string,Action>())
        //s2
        (fun (dict : Dictionary<string,Action>) ->
            dict.Add("comma",Error "expected 'operator','power','parentheses','times','eoi', but given 'comma'")
            dict.Add("divide",Reduce 7)
            dict.Add("float",Error "expected 'operator','power','parentheses','times','eoi', but given 'operand'")
            dict.Add("id",Error "expected 'operator','power','parentheses','times','eoi', but given 'operand'")
            dict.Add("int",Error "expected 'operator','power','parentheses','times','eoi', but given 'operand'")
            dict.Add("lpar",Error "expected 'operator','power','parentheses','times','eoi', but given 'parentheses'")
            dict.Add("minus",Reduce 7)
            dict.Add("plus",Reduce 7)
            dict.Add("power",Reduce 7)
            dict.Add("rpar",Reduce 7)
            dict.Add("times",Reduce 7)
            dict.Add("$",Reduce 7)
            dict
            )(new Dictionary<string,Action>())
        //s3
        (fun (dict : Dictionary<string,Action>) ->
            dict.Add("comma",Error "expected 'operator','power','times','eoi', but given 'comma'")
            dict.Add("divide",Shift 10)
            dict.Add("float",Error "expected 'operator','power','times','eoi', but given 'operand'")
            dict.Add("id",Error "expected 'operator','power','times','eoi', but given 'operand'")
            dict.Add("int",Error "expected 'operator','power','times','eoi', but given 'operand'")
            dict.Add("lpar",Error "expected 'operator','power','times','eoi', but given 'parentheses'")
            dict.Add("minus",Shift 11)
            dict.Add("plus",Shift 12)
            dict.Add("power",Shift 13)
            dict.Add("rpar",Error "expected 'operator','power','times','eoi', but given 'parentheses'")
            dict.Add("times",Shift 14)
            dict.Add("$",Reduce 1)
            dict
            )(new Dictionary<string,Action>())
        //s4
        (fun (dict : Dictionary<string,Action>) ->
            dict.Add("comma",Error "expected 'eoi', but given 'comma'")
            dict.Add("divide",Error "expected 'eoi', but given 'operator'")
            dict.Add("float",Error "expected 'eoi', but given 'operand'")
            dict.Add("id",Error "expected 'eoi', but given 'operand'")
            dict.Add("int",Error "expected 'eoi', but given 'operand'")
            dict.Add("lpar",Error "expected 'eoi', but given 'parentheses'")
            dict.Add("minus",Error "expected 'eoi', but given 'operator'")
            dict.Add("plus",Error "expected 'eoi', but given 'operator'")
            dict.Add("power",Error "expected 'eoi', but given 'power'")
            dict.Add("rpar",Error "expected 'eoi', but given 'parentheses'")
            dict.Add("times",Error "expected 'eoi', but given 'times'")
            dict.Add("$",Accept)
            dict
            )(new Dictionary<string,Action>())
        //s5
        (fun (dict : Dictionary<string,Action>) ->
            dict.Add("comma",Reduce 17)
            dict.Add("divide",Reduce 17)
            dict.Add("float",Error "expected 'comma','operator','power','parentheses','times','eoi', but given 'operand'")
            dict.Add("id",Error "expected 'comma','operator','power','parentheses','times','eoi', but given 'operand'")
            dict.Add("int",Error "expected 'comma','operator','power','parentheses','times','eoi', but given 'operand'")
            dict.Add("lpar",Error "expected 'comma','operator','power','parentheses','times','eoi', but given 'parentheses'")
            dict.Add("minus",Reduce 17)
            dict.Add("plus",Reduce 17)
            dict.Add("power",Reduce 17)
            dict.Add("rpar",Reduce 17)
            dict.Add("times",Reduce 17)
            dict.Add("$",Reduce 17)
            dict
            )(new Dictionary<string,Action>())
        //s6
        (fun (dict : Dictionary<string,Action>) ->
            dict.Add("comma",Error "expected 'parentheses', but given 'comma'")
            dict.Add("divide",Error "expected 'parentheses', but given 'operator'")
            dict.Add("float",Error "expected 'parentheses', but given 'operand'")
            dict.Add("id",Error "expected 'parentheses', but given 'operand'")
            dict.Add("int",Error "expected 'parentheses', but given 'operand'")
            dict.Add("lpar",Shift 15)
            dict.Add("minus",Error "expected 'parentheses', but given 'operator'")
            dict.Add("plus",Error "expected 'parentheses', but given 'operator'")
            dict.Add("power",Error "expected 'parentheses', but given 'power'")
            dict.Add("rpar",Error "expected 'parentheses', but given 'parentheses'")
            dict.Add("times",Error "expected 'parentheses', but given 'times'")
            dict.Add("$",Error "expected 'parentheses', but given '$'")
            dict
            )(new Dictionary<string,Action>())
        //s7
        (fun (dict : Dictionary<string,Action>) ->
            dict.Add("comma",Reduce 18)
            dict.Add("divide",Reduce 18)
            dict.Add("float",Error "expected 'comma','operator','power','parentheses','times','eoi', but given 'operand'")
            dict.Add("id",Error "expected 'comma','operator','power','parentheses','times','eoi', but given 'operand'")
            dict.Add("int",Error "expected 'comma','operator','power','parentheses','times','eoi', but given 'operand'")
            dict.Add("lpar",Error "expected 'comma','operator','power','parentheses','times','eoi', but given 'parentheses'")
            dict.Add("minus",Reduce 18)
            dict.Add("plus",Reduce 18)
            dict.Add("power",Reduce 18)
            dict.Add("rpar",Reduce 18)
            dict.Add("times",Reduce 18)
            dict.Add("$",Reduce 18)
            dict
            )(new Dictionary<string,Action>())
        //s8
        (fun (dict : Dictionary<string,Action>) ->
            dict.Add("comma",Error "expected 'operand','parentheses','operator', but given 'comma'")
            dict.Add("divide",Error "expected 'operand','parentheses','operator', but given 'operator'")
            dict.Add("float",Shift 5)
            dict.Add("id",Shift 6)
            dict.Add("int",Shift 7)
            dict.Add("lpar",Shift 8)
            dict.Add("minus",Shift 9)
            dict.Add("plus",Error "expected 'operand','parentheses','operator', but given 'operator'")
            dict.Add("power",Error "expected 'operand','parentheses','operator', but given 'power'")
            dict.Add("rpar",Error "expected 'operand','parentheses','operator', but given 'parentheses'")
            dict.Add("times",Error "expected 'operand','parentheses','operator', but given 'times'")
            dict.Add("$",Error "expected 'operand','parentheses','operator', but given '$'")
            dict
            )(new Dictionary<string,Action>())
        //s9
        (fun (dict : Dictionary<string,Action>) ->
            dict.Add("comma",Error "expected 'operand','parentheses','operator', but given 'comma'")
            dict.Add("divide",Error "expected 'operand','parentheses','operator', but given 'operator'")
            dict.Add("float",Shift 5)
            dict.Add("id",Shift 6)
            dict.Add("int",Shift 7)
            dict.Add("lpar",Shift 8)
            dict.Add("minus",Shift 9)
            dict.Add("plus",Error "expected 'operand','parentheses','operator', but given 'operator'")
            dict.Add("power",Error "expected 'operand','parentheses','operator', but given 'power'")
            dict.Add("rpar",Error "expected 'operand','parentheses','operator', but given 'parentheses'")
            dict.Add("times",Error "expected 'operand','parentheses','operator', but given 'times'")
            dict.Add("$",Error "expected 'operand','parentheses','operator', but given '$'")
            dict
            )(new Dictionary<string,Action>())
        //s10
        (fun (dict : Dictionary<string,Action>) ->
            dict.Add("comma",Error "expected 'operand','parentheses','operator', but given 'comma'")
            dict.Add("divide",Error "expected 'operand','parentheses','operator', but given 'operator'")
            dict.Add("float",Shift 5)
            dict.Add("id",Shift 6)
            dict.Add("int",Shift 7)
            dict.Add("lpar",Shift 8)
            dict.Add("minus",Shift 9)
            dict.Add("plus",Error "expected 'operand','parentheses','operator', but given 'operator'")
            dict.Add("power",Error "expected 'operand','parentheses','operator', but given 'power'")
            dict.Add("rpar",Error "expected 'operand','parentheses','operator', but given 'parentheses'")
            dict.Add("times",Error "expected 'operand','parentheses','operator', but given 'times'")
            dict.Add("$",Error "expected 'operand','parentheses','operator', but given '$'")
            dict
            )(new Dictionary<string,Action>())
        //s11
        (fun (dict : Dictionary<string,Action>) ->
            dict.Add("comma",Error "expected 'operand','parentheses','operator', but given 'comma'")
            dict.Add("divide",Error "expected 'operand','parentheses','operator', but given 'operator'")
            dict.Add("float",Shift 5)
            dict.Add("id",Shift 6)
            dict.Add("int",Shift 7)
            dict.Add("lpar",Shift 8)
            dict.Add("minus",Shift 9)
            dict.Add("plus",Error "expected 'operand','parentheses','operator', but given 'operator'")
            dict.Add("power",Error "expected 'operand','parentheses','operator', but given 'power'")
            dict.Add("rpar",Error "expected 'operand','parentheses','operator', but given 'parentheses'")
            dict.Add("times",Error "expected 'operand','parentheses','operator', but given 'times'")
            dict.Add("$",Error "expected 'operand','parentheses','operator', but given '$'")
            dict
            )(new Dictionary<string,Action>())
        //s12
        (fun (dict : Dictionary<string,Action>) ->
            dict.Add("comma",Error "expected 'operand','parentheses','operator', but given 'comma'")
            dict.Add("divide",Error "expected 'operand','parentheses','operator', but given 'operator'")
            dict.Add("float",Shift 5)
            dict.Add("id",Shift 6)
            dict.Add("int",Shift 7)
            dict.Add("lpar",Shift 8)
            dict.Add("minus",Shift 9)
            dict.Add("plus",Error "expected 'operand','parentheses','operator', but given 'operator'")
            dict.Add("power",Error "expected 'operand','parentheses','operator', but given 'power'")
            dict.Add("rpar",Error "expected 'operand','parentheses','operator', but given 'parentheses'")
            dict.Add("times",Error "expected 'operand','parentheses','operator', but given 'times'")
            dict.Add("$",Error "expected 'operand','parentheses','operator', but given '$'")
            dict
            )(new Dictionary<string,Action>())
        //s13
        (fun (dict : Dictionary<string,Action>) ->
            dict.Add("comma",Error "expected 'operand','parentheses','operator', but given 'comma'")
            dict.Add("divide",Error "expected 'operand','parentheses','operator', but given 'operator'")
            dict.Add("float",Shift 5)
            dict.Add("id",Shift 6)
            dict.Add("int",Shift 7)
            dict.Add("lpar",Shift 8)
            dict.Add("minus",Shift 9)
            dict.Add("plus",Error "expected 'operand','parentheses','operator', but given 'operator'")
            dict.Add("power",Error "expected 'operand','parentheses','operator', but given 'power'")
            dict.Add("rpar",Error "expected 'operand','parentheses','operator', but given 'parentheses'")
            dict.Add("times",Error "expected 'operand','parentheses','operator', but given 'times'")
            dict.Add("$",Error "expected 'operand','parentheses','operator', but given '$'")
            dict
            )(new Dictionary<string,Action>())
        //s14
        (fun (dict : Dictionary<string,Action>) ->
            dict.Add("comma",Error "expected 'operand','parentheses','operator', but given 'comma'")
            dict.Add("divide",Error "expected 'operand','parentheses','operator', but given 'operator'")
            dict.Add("float",Shift 5)
            dict.Add("id",Shift 6)
            dict.Add("int",Shift 7)
            dict.Add("lpar",Shift 8)
            dict.Add("minus",Shift 9)
            dict.Add("plus",Error "expected 'operand','parentheses','operator', but given 'operator'")
            dict.Add("power",Error "expected 'operand','parentheses','operator', but given 'power'")
            dict.Add("rpar",Error "expected 'operand','parentheses','operator', but given 'parentheses'")
            dict.Add("times",Error "expected 'operand','parentheses','operator', but given 'times'")
            dict.Add("$",Error "expected 'operand','parentheses','operator', but given '$'")
            dict
            )(new Dictionary<string,Action>())
        //s15
        (fun (dict : Dictionary<string,Action>) ->
            dict.Add("comma",Reduce 15)
            dict.Add("divide",Reduce 15)
            dict.Add("float",Shift 5)
            dict.Add("id",Shift 6)
            dict.Add("int",Shift 7)
            dict.Add("lpar",Shift 8)
            dict.Add("minus",Reduce 15)
            dict.Add("plus",Reduce 15)
            dict.Add("power",Reduce 15)
            dict.Add("rpar",Reduce 15)
            dict.Add("times",Reduce 15)
            dict.Add("$",Reduce 15)
            dict
            )(new Dictionary<string,Action>())
        //s16
        (fun (dict : Dictionary<string,Action>) ->
            dict.Add("comma",Error "expected 'operator','power','parentheses','times', but given 'comma'")
            dict.Add("divide",Shift 10)
            dict.Add("float",Error "expected 'operator','power','parentheses','times', but given 'operand'")
            dict.Add("id",Error "expected 'operator','power','parentheses','times', but given 'operand'")
            dict.Add("int",Error "expected 'operator','power','parentheses','times', but given 'operand'")
            dict.Add("lpar",Error "expected 'operator','power','parentheses','times', but given 'parentheses'")
            dict.Add("minus",Shift 11)
            dict.Add("plus",Shift 12)
            dict.Add("power",Shift 13)
            dict.Add("rpar",Shift 27)
            dict.Add("times",Shift 14)
            dict.Add("$",Error "expected 'operator','power','parentheses','times', but given '$'")
            dict
            )(new Dictionary<string,Action>())
        //s17
        (fun (dict : Dictionary<string,Action>) ->
            dict.Add("comma",Error "expected 'operator','power','parentheses','times','eoi', but given 'comma'")
            dict.Add("divide",s10)
            dict.Add("float",Error "expected 'operator','power','parentheses','times','eoi', but given 'operand'")
            dict.Add("id",Error "expected 'operator','power','parentheses','times','eoi', but given 'operand'")
            dict.Add("int",Error "expected 'operator','power','parentheses','times','eoi', but given 'operand'")
            dict.Add("lpar",Error "expected 'operator','power','parentheses','times','eoi', but given 'parentheses'")
            dict.Add("minus",r9)
            dict.Add("plus",r9)
            dict.Add("power",s13)
            dict.Add("rpar",Reduce 9)
            dict.Add("times",s14)
            dict.Add("$",Reduce 9)
            dict
            )(new Dictionary<string,Action>())
        //s18
        (fun (dict : Dictionary<string,Action>) ->
            dict.Add("comma",Error "expected 'operator','power','parentheses','times','eoi', but given 'comma'")
            dict.Add("divide",r5)
            dict.Add("float",Error "expected 'operator','power','parentheses','times','eoi', but given 'operand'")
            dict.Add("id",Error "expected 'operator','power','parentheses','times','eoi', but given 'operand'")
            dict.Add("int",Error "expected 'operator','power','parentheses','times','eoi', but given 'operand'")
            dict.Add("lpar",Error "expected 'operator','power','parentheses','times','eoi', but given 'parentheses'")
            dict.Add("minus",r5)
            dict.Add("plus",r5)
            dict.Add("power",s13)
            dict.Add("rpar",Reduce 5)
            dict.Add("times",r5)
            dict.Add("$",Reduce 5)
            dict
            )(new Dictionary<string,Action>())
        //s19
        (fun (dict : Dictionary<string,Action>) ->
            dict.Add("comma",Error "expected 'operator','power','parentheses','times','eoi', but given 'comma'")
            dict.Add("divide",s10)
            dict.Add("float",Error "expected 'operator','power','parentheses','times','eoi', but given 'operand'")
            dict.Add("id",Error "expected 'operator','power','parentheses','times','eoi', but given 'operand'")
            dict.Add("int",Error "expected 'operator','power','parentheses','times','eoi', but given 'operand'")
            dict.Add("lpar",Error "expected 'operator','power','parentheses','times','eoi', but given 'parentheses'")
            dict.Add("minus",r2)
            dict.Add("plus",r2)
            dict.Add("power",s13)
            dict.Add("rpar",Reduce 2)
            dict.Add("times",s14)
            dict.Add("$",Reduce 2)
            dict
            )(new Dictionary<string,Action>())
        //s20
        (fun (dict : Dictionary<string,Action>) ->
            dict.Add("comma",Error "expected 'operator','power','parentheses','times','eoi', but given 'comma'")
            dict.Add("divide",s10)
            dict.Add("float",Error "expected 'operator','power','parentheses','times','eoi', but given 'operand'")
            dict.Add("id",Error "expected 'operator','power','parentheses','times','eoi', but given 'operand'")
            dict.Add("int",Error "expected 'operator','power','parentheses','times','eoi', but given 'operand'")
            dict.Add("lpar",Error "expected 'operator','power','parentheses','times','eoi', but given 'parentheses'")
            dict.Add("minus",r3)
            dict.Add("plus",r3)
            dict.Add("power",s13)
            dict.Add("rpar",Reduce 3)
            dict.Add("times",s14)
            dict.Add("$",Reduce 3)
            dict
            )(new Dictionary<string,Action>())
        //s21
        (fun (dict : Dictionary<string,Action>) ->
            dict.Add("comma",Error "expected 'operator','power','parentheses','times','eoi', but given 'comma'")
            dict.Add("divide",r6)
            dict.Add("float",Error "expected 'operator','power','parentheses','times','eoi', but given 'operand'")
            dict.Add("id",Error "expected 'operator','power','parentheses','times','eoi', but given 'operand'")
            dict.Add("int",Error "expected 'operator','power','parentheses','times','eoi', but given 'operand'")
            dict.Add("lpar",Error "expected 'operator','power','parentheses','times','eoi', but given 'parentheses'")
            dict.Add("minus",r6)
            dict.Add("plus",r6)
            dict.Add("power",s13)
            dict.Add("rpar",Reduce 6)
            dict.Add("times",r6)
            dict.Add("$",Reduce 6)
            dict
            )(new Dictionary<string,Action>())
        //s22
        (fun (dict : Dictionary<string,Action>) ->
            dict.Add("comma",Error "expected 'operator','power','parentheses','times','eoi', but given 'comma'")
            dict.Add("divide",r4)
            dict.Add("float",Error "expected 'operator','power','parentheses','times','eoi', but given 'operand'")
            dict.Add("id",Error "expected 'operator','power','parentheses','times','eoi', but given 'operand'")
            dict.Add("int",Error "expected 'operator','power','parentheses','times','eoi', but given 'operand'")
            dict.Add("lpar",Error "expected 'operator','power','parentheses','times','eoi', but given 'parentheses'")
            dict.Add("minus",r4)
            dict.Add("plus",r4)
            dict.Add("power",s13)
            dict.Add("rpar",Reduce 4)
            dict.Add("times",r4)
            dict.Add("$",Reduce 4)
            dict
            )(new Dictionary<string,Action>())
        //s23
        (fun (dict : Dictionary<string,Action>) ->
            dict.Add("comma",Shift 28)
            dict.Add("divide",Error "expected 'comma','parentheses', but given 'operator'")
            dict.Add("float",Error "expected 'comma','parentheses', but given 'operand'")
            dict.Add("id",Error "expected 'comma','parentheses', but given 'operand'")
            dict.Add("int",Error "expected 'comma','parentheses', but given 'operand'")
            dict.Add("lpar",Error "expected 'comma','parentheses', but given 'parentheses'")
            dict.Add("minus",Error "expected 'comma','parentheses', but given 'operator'")
            dict.Add("plus",Error "expected 'comma','parentheses', but given 'operator'")
            dict.Add("power",Error "expected 'comma','parentheses', but given 'power'")
            dict.Add("rpar",Reduce 11)
            dict.Add("times",Error "expected 'comma','parentheses', but given 'times'")
            dict.Add("$",Error "expected 'comma','parentheses', but given '$'")
            dict
            )(new Dictionary<string,Action>())
        //s24
        (fun (dict : Dictionary<string,Action>) ->
            dict.Add("comma",Error "expected 'parentheses', but given 'comma'")
            dict.Add("divide",Error "expected 'parentheses', but given 'operator'")
            dict.Add("float",Error "expected 'parentheses', but given 'operand'")
            dict.Add("id",Error "expected 'parentheses', but given 'operand'")
            dict.Add("int",Error "expected 'parentheses', but given 'operand'")
            dict.Add("lpar",Error "expected 'parentheses', but given 'parentheses'")
            dict.Add("minus",Error "expected 'parentheses', but given 'operator'")
            dict.Add("plus",Error "expected 'parentheses', but given 'operator'")
            dict.Add("power",Error "expected 'parentheses', but given 'power'")
            dict.Add("rpar",Shift 29)
            dict.Add("times",Error "expected 'parentheses', but given 'times'")
            dict.Add("$",Error "expected 'parentheses', but given '$'")
            dict
            )(new Dictionary<string,Action>())
        //s25
        (fun (dict : Dictionary<string,Action>) ->
            dict.Add("comma",Reduce 13)
            dict.Add("divide",Reduce 13)
            dict.Add("float",Error "expected 'comma','operator','power','parentheses','times','eoi', but given 'operand'")
            dict.Add("id",Error "expected 'comma','operator','power','parentheses','times','eoi', but given 'operand'")
            dict.Add("int",Error "expected 'comma','operator','power','parentheses','times','eoi', but given 'operand'")
            dict.Add("lpar",Error "expected 'comma','operator','power','parentheses','times','eoi', but given 'parentheses'")
            dict.Add("minus",Reduce 13)
            dict.Add("plus",Reduce 13)
            dict.Add("power",Reduce 13)
            dict.Add("rpar",Reduce 13)
            dict.Add("times",Reduce 13)
            dict.Add("$",Reduce 13)
            dict
            )(new Dictionary<string,Action>())
        //s26
        (fun (dict : Dictionary<string,Action>) ->
            dict.Add("comma",Reduce 14)
            dict.Add("divide",Reduce 14)
            dict.Add("float",Error "expected 'comma','operator','power','parentheses','times','eoi', but given 'operand'")
            dict.Add("id",Error "expected 'comma','operator','power','parentheses','times','eoi', but given 'operand'")
            dict.Add("int",Error "expected 'comma','operator','power','parentheses','times','eoi', but given 'operand'")
            dict.Add("lpar",Error "expected 'comma','operator','power','parentheses','times','eoi', but given 'parentheses'")
            dict.Add("minus",Reduce 14)
            dict.Add("plus",Reduce 14)
            dict.Add("power",Reduce 14)
            dict.Add("rpar",Reduce 14)
            dict.Add("times",Reduce 14)
            dict.Add("$",Reduce 14)
            dict
            )(new Dictionary<string,Action>())
        //s27
        (fun (dict : Dictionary<string,Action>) ->
            dict.Add("comma",Reduce 16)
            dict.Add("divide",Reduce 16)
            dict.Add("float",Error "expected 'comma','operator','power','parentheses','times','eoi', but given 'operand'")
            dict.Add("id",Error "expected 'comma','operator','power','parentheses','times','eoi', but given 'operand'")
            dict.Add("int",Error "expected 'comma','operator','power','parentheses','times','eoi', but given 'operand'")
            dict.Add("lpar",Error "expected 'comma','operator','power','parentheses','times','eoi', but given 'parentheses'")
            dict.Add("minus",Reduce 16)
            dict.Add("plus",Reduce 16)
            dict.Add("power",Reduce 16)
            dict.Add("rpar",Reduce 16)
            dict.Add("times",Reduce 16)
            dict.Add("$",Reduce 16)
            dict
            )(new Dictionary<string,Action>())
        //s28
        (fun (dict : Dictionary<string,Action>) ->
            dict.Add("comma",Reduce 15)
            dict.Add("divide",Reduce 15)
            dict.Add("float",Shift 5)
            dict.Add("id",Shift 6)
            dict.Add("int",Shift 7)
            dict.Add("lpar",Shift 8)
            dict.Add("minus",Reduce 15)
            dict.Add("plus",Reduce 15)
            dict.Add("power",Reduce 15)
            dict.Add("rpar",Reduce 15)
            dict.Add("times",Reduce 15)
            dict.Add("$",Reduce 15)
            dict
            )(new Dictionary<string,Action>())
        //s29
        (fun (dict : Dictionary<string,Action>) ->
            dict.Add("comma",Reduce 10)
            dict.Add("divide",Reduce 10)
            dict.Add("float",Error "expected 'comma','operator','power','parentheses','times','eoi', but given 'operand'")
            dict.Add("id",Error "expected 'comma','operator','power','parentheses','times','eoi', but given 'operand'")
            dict.Add("int",Error "expected 'comma','operator','power','parentheses','times','eoi', but given 'operand'")
            dict.Add("lpar",Error "expected 'comma','operator','power','parentheses','times','eoi', but given 'parentheses'")
            dict.Add("minus",Reduce 10)
            dict.Add("plus",Reduce 10)
            dict.Add("power",Reduce 10)
            dict.Add("rpar",Reduce 10)
            dict.Add("times",Reduce 10)
            dict.Add("$",Reduce 10)
            dict
            )(new Dictionary<string,Action>())
        //s30
        (fun (dict : Dictionary<string,Action>) ->
            dict.Add("comma",Error "expected 'parentheses', but given 'comma'")
            dict.Add("divide",Error "expected 'parentheses', but given 'operator'")
            dict.Add("float",Error "expected 'parentheses', but given 'operand'")
            dict.Add("id",Error "expected 'parentheses', but given 'operand'")
            dict.Add("int",Error "expected 'parentheses', but given 'operand'")
            dict.Add("lpar",Error "expected 'parentheses', but given 'parentheses'")
            dict.Add("minus",Error "expected 'parentheses', but given 'operator'")
            dict.Add("plus",Error "expected 'parentheses', but given 'operator'")
            dict.Add("power",Error "expected 'parentheses', but given 'power'")
            dict.Add("rpar",Reduce 12)
            dict.Add("times",Error "expected 'parentheses', but given 'times'")
            dict.Add("$",Error "expected 'parentheses', but given '$'")
            dict
            )(new Dictionary<string,Action>())
        |]
    let gotoTable = [|
        //s0
        (fun (dict : Dictionary<string,Option<int>>) ->
            dict.Add("Arg",None)
            dict.Add("Args",None)
            dict.Add("Atom",Some 1)
            dict.Add("Call",Some 2)
            dict.Add("Exp",Some 3)
            dict.Add("Exp'",Some 4)
            dict
            )(new Dictionary<string,Option<int>>())
        //s1
        (fun (dict : Dictionary<string,Option<int>>) ->
            dict.Add("Arg",None)
            dict.Add("Args",None)
            dict.Add("Atom",None)
            dict.Add("Call",None)
            dict.Add("Exp",None)
            dict.Add("Exp'",None)
            dict
            )(new Dictionary<string,Option<int>>())
        //s2
        (fun (dict : Dictionary<string,Option<int>>) ->
            dict.Add("Arg",None)
            dict.Add("Args",None)
            dict.Add("Atom",None)
            dict.Add("Call",None)
            dict.Add("Exp",None)
            dict.Add("Exp'",None)
            dict
            )(new Dictionary<string,Option<int>>())
        //s3
        (fun (dict : Dictionary<string,Option<int>>) ->
            dict.Add("Arg",None)
            dict.Add("Args",None)
            dict.Add("Atom",None)
            dict.Add("Call",None)
            dict.Add("Exp",None)
            dict.Add("Exp'",None)
            dict
            )(new Dictionary<string,Option<int>>())
        //s4
        (fun (dict : Dictionary<string,Option<int>>) ->
            dict.Add("Arg",None)
            dict.Add("Args",None)
            dict.Add("Atom",None)
            dict.Add("Call",None)
            dict.Add("Exp",None)
            dict.Add("Exp'",None)
            dict
            )(new Dictionary<string,Option<int>>())
        //s5
        (fun (dict : Dictionary<string,Option<int>>) ->
            dict.Add("Arg",None)
            dict.Add("Args",None)
            dict.Add("Atom",None)
            dict.Add("Call",None)
            dict.Add("Exp",None)
            dict.Add("Exp'",None)
            dict
            )(new Dictionary<string,Option<int>>())
        //s6
        (fun (dict : Dictionary<string,Option<int>>) ->
            dict.Add("Arg",None)
            dict.Add("Args",None)
            dict.Add("Atom",None)
            dict.Add("Call",None)
            dict.Add("Exp",None)
            dict.Add("Exp'",None)
            dict
            )(new Dictionary<string,Option<int>>())
        //s7
        (fun (dict : Dictionary<string,Option<int>>) ->
            dict.Add("Arg",None)
            dict.Add("Args",None)
            dict.Add("Atom",None)
            dict.Add("Call",None)
            dict.Add("Exp",None)
            dict.Add("Exp'",None)
            dict
            )(new Dictionary<string,Option<int>>())
        //s8
        (fun (dict : Dictionary<string,Option<int>>) ->
            dict.Add("Arg",None)
            dict.Add("Args",None)
            dict.Add("Atom",Some 1)
            dict.Add("Call",Some 2)
            dict.Add("Exp",Some 16)
            dict.Add("Exp'",None)
            dict
            )(new Dictionary<string,Option<int>>())
        //s9
        (fun (dict : Dictionary<string,Option<int>>) ->
            dict.Add("Arg",None)
            dict.Add("Args",None)
            dict.Add("Atom",Some 1)
            dict.Add("Call",Some 2)
            dict.Add("Exp",Some 17)
            dict.Add("Exp'",None)
            dict
            )(new Dictionary<string,Option<int>>())
        //s10
        (fun (dict : Dictionary<string,Option<int>>) ->
            dict.Add("Arg",None)
            dict.Add("Args",None)
            dict.Add("Atom",Some 1)
            dict.Add("Call",Some 2)
            dict.Add("Exp",Some 18)
            dict.Add("Exp'",None)
            dict
            )(new Dictionary<string,Option<int>>())
        //s11
        (fun (dict : Dictionary<string,Option<int>>) ->
            dict.Add("Arg",None)
            dict.Add("Args",None)
            dict.Add("Atom",Some 1)
            dict.Add("Call",Some 2)
            dict.Add("Exp",Some 19)
            dict.Add("Exp'",None)
            dict
            )(new Dictionary<string,Option<int>>())
        //s12
        (fun (dict : Dictionary<string,Option<int>>) ->
            dict.Add("Arg",None)
            dict.Add("Args",None)
            dict.Add("Atom",Some 1)
            dict.Add("Call",Some 2)
            dict.Add("Exp",Some 20)
            dict.Add("Exp'",None)
            dict
            )(new Dictionary<string,Option<int>>())
        //s13
        (fun (dict : Dictionary<string,Option<int>>) ->
            dict.Add("Arg",None)
            dict.Add("Args",None)
            dict.Add("Atom",Some 1)
            dict.Add("Call",Some 2)
            dict.Add("Exp",Some 21)
            dict.Add("Exp'",None)
            dict
            )(new Dictionary<string,Option<int>>())
        //s14
        (fun (dict : Dictionary<string,Option<int>>) ->
            dict.Add("Arg",None)
            dict.Add("Args",None)
            dict.Add("Atom",Some 1)
            dict.Add("Call",Some 2)
            dict.Add("Exp",Some 22)
            dict.Add("Exp'",None)
            dict
            )(new Dictionary<string,Option<int>>())
        //s15
        (fun (dict : Dictionary<string,Option<int>>) ->
            dict.Add("Arg",Some 23)
            dict.Add("Args",Some 24)
            dict.Add("Atom",Some 25)
            dict.Add("Call",Some 26)
            dict.Add("Exp",None)
            dict.Add("Exp'",None)
            dict
            )(new Dictionary<string,Option<int>>())
        //s16
        (fun (dict : Dictionary<string,Option<int>>) ->
            dict.Add("Arg",None)
            dict.Add("Args",None)
            dict.Add("Atom",None)
            dict.Add("Call",None)
            dict.Add("Exp",None)
            dict.Add("Exp'",None)
            dict
            )(new Dictionary<string,Option<int>>())
        //s17
        (fun (dict : Dictionary<string,Option<int>>) ->
            dict.Add("Arg",None)
            dict.Add("Args",None)
            dict.Add("Atom",None)
            dict.Add("Call",None)
            dict.Add("Exp",None)
            dict.Add("Exp'",None)
            dict
            )(new Dictionary<string,Option<int>>())
        //s18
        (fun (dict : Dictionary<string,Option<int>>) ->
            dict.Add("Arg",None)
            dict.Add("Args",None)
            dict.Add("Atom",None)
            dict.Add("Call",None)
            dict.Add("Exp",None)
            dict.Add("Exp'",None)
            dict
            )(new Dictionary<string,Option<int>>())
        //s19
        (fun (dict : Dictionary<string,Option<int>>) ->
            dict.Add("Arg",None)
            dict.Add("Args",None)
            dict.Add("Atom",None)
            dict.Add("Call",None)
            dict.Add("Exp",None)
            dict.Add("Exp'",None)
            dict
            )(new Dictionary<string,Option<int>>())
        //s20
        (fun (dict : Dictionary<string,Option<int>>) ->
            dict.Add("Arg",None)
            dict.Add("Args",None)
            dict.Add("Atom",None)
            dict.Add("Call",None)
            dict.Add("Exp",None)
            dict.Add("Exp'",None)
            dict
            )(new Dictionary<string,Option<int>>())
        //s21
        (fun (dict : Dictionary<string,Option<int>>) ->
            dict.Add("Arg",None)
            dict.Add("Args",None)
            dict.Add("Atom",None)
            dict.Add("Call",None)
            dict.Add("Exp",None)
            dict.Add("Exp'",None)
            dict
            )(new Dictionary<string,Option<int>>())
        //s22
        (fun (dict : Dictionary<string,Option<int>>) ->
            dict.Add("Arg",None)
            dict.Add("Args",None)
            dict.Add("Atom",None)
            dict.Add("Call",None)
            dict.Add("Exp",None)
            dict.Add("Exp'",None)
            dict
            )(new Dictionary<string,Option<int>>())
        //s23
        (fun (dict : Dictionary<string,Option<int>>) ->
            dict.Add("Arg",None)
            dict.Add("Args",None)
            dict.Add("Atom",None)
            dict.Add("Call",None)
            dict.Add("Exp",None)
            dict.Add("Exp'",None)
            dict
            )(new Dictionary<string,Option<int>>())
        //s24
        (fun (dict : Dictionary<string,Option<int>>) ->
            dict.Add("Arg",None)
            dict.Add("Args",None)
            dict.Add("Atom",None)
            dict.Add("Call",None)
            dict.Add("Exp",None)
            dict.Add("Exp'",None)
            dict
            )(new Dictionary<string,Option<int>>())
        //s25
        (fun (dict : Dictionary<string,Option<int>>) ->
            dict.Add("Arg",None)
            dict.Add("Args",None)
            dict.Add("Atom",None)
            dict.Add("Call",None)
            dict.Add("Exp",None)
            dict.Add("Exp'",None)
            dict
            )(new Dictionary<string,Option<int>>())
        //s26
        (fun (dict : Dictionary<string,Option<int>>) ->
            dict.Add("Arg",None)
            dict.Add("Args",None)
            dict.Add("Atom",None)
            dict.Add("Call",None)
            dict.Add("Exp",None)
            dict.Add("Exp'",None)
            dict
            )(new Dictionary<string,Option<int>>())
        //s27
        (fun (dict : Dictionary<string,Option<int>>) ->
            dict.Add("Arg",None)
            dict.Add("Args",None)
            dict.Add("Atom",None)
            dict.Add("Call",None)
            dict.Add("Exp",None)
            dict.Add("Exp'",None)
            dict
            )(new Dictionary<string,Option<int>>())
        //s28
        (fun (dict : Dictionary<string,Option<int>>) ->
            dict.Add("Arg",Some 23)
            dict.Add("Args",Some 30)
            dict.Add("Atom",Some 25)
            dict.Add("Call",Some 26)
            dict.Add("Exp",None)
            dict.Add("Exp'",None)
            dict
            )(new Dictionary<string,Option<int>>())
        //s29
        (fun (dict : Dictionary<string,Option<int>>) ->
            dict.Add("Arg",None)
            dict.Add("Args",None)
            dict.Add("Atom",None)
            dict.Add("Call",None)
            dict.Add("Exp",None)
            dict.Add("Exp'",None)
            dict
            )(new Dictionary<string,Option<int>>())
        //s30
        (fun (dict : Dictionary<string,Option<int>>) ->
            dict.Add("Arg",None)
            dict.Add("Args",None)
            dict.Add("Atom",None)
            dict.Add("Call",None)
            dict.Add("Exp",None)
            dict.Add("Exp'",None)
            dict
            )(new Dictionary<string,Option<int>>())
        |]
    let productions_str = [|
        ("__",[|"Exp'"|]) //0
        ("Exp'",[|"Exp"|]) //1
        ("Exp",[|"Exp";"minus";"Exp"|]) //2
        ("Exp",[|"Exp";"plus";"Exp"|]) //3
        ("Exp",[|"Exp";"times";"Exp"|]) //4
        ("Exp",[|"Exp";"divide";"Exp"|]) //5
        ("Exp",[|"Exp";"power";"Exp"|]) //6
        ("Exp",[|"Call"|]) //7
        ("Exp",[|"Atom"|]) //8
        ("Exp",[|"minus";"Exp"|]) //9
        ("Call",[|"id";"lpar";"Args";"rpar"|]) //10
        ("Args",[|"Arg"|]) //11
        ("Args",[|"Arg";"comma";"Args"|]) //12
        ("Arg",[|"Atom"|]) //13
        ("Arg",[|"Call"|]) //14
        ("Arg",[||]) //15
        ("Atom",[|"lpar";"Exp";"rpar"|]) //16
        ("Atom",[|"float"|]) //17
        ("Atom",[|"int"|]) //18
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
        let rec exec (i,a) sStack tree =
            let (s,_) = popOne sStack
            match actionTable.[s].[fst a] with
            | Shift t ->
                let newStack = t::sStack
                let newTree = addLeaf2tree tree a
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
        exec (0,tokens.[0]) [0] initTreeStack
    let parse str =
        let lexed = lexer str
        let parsed = parser lexed
        parsed
