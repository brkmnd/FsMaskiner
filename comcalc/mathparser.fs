namespace Maskiner
module MathParser =
    open System.Collections.Generic
    open System.Text.RegularExpressions
    type Action =
        | Shift of int
        | Reduce of int
        | Accept
        | Error of string
    type Tree =
        | EmptyTree
        | ArgResult of float
        | CallArgs of float list
        | Result of float
        | FuncId of string
        | ConstResult of float
    let addToken2tree tree node =
        match node with
        | ("float",Some v,pos) | ("int",Some v,pos) -> (ConstResult (float v))::tree
        | ("id",Some v,pos) -> (FuncId v)::tree
        | _ -> tree
    let productions_fun = [|
        //[0] __ -> Exp 
        (fun tree ftab -> tree)
        //[1] Exp -> Exp minus Exp 
        (fun tree ftab ->
            match tree with
            | (Result r)::(Result l)::tree ->
                (Result (l - r))::tree
            | _ -> tree
            )
        //[2] Exp -> Exp plus Exp 
        (fun tree ftab ->
            match tree with
            | (Result r)::(Result l)::tree ->
                (Result (l + r))::tree
            | _ -> tree
            )
        //[3] Exp -> Exp times Exp 
        (fun tree ftab ->
            match tree with
            | (Result r)::(Result l)::tree ->
                (Result (l * r))::tree
            | _ -> tree
            )
        //[4] Exp -> Exp divide Exp 
        (fun tree ftab ->
            match tree with
            | (Result r)::(Result l)::tree ->
                (Result (l / r))::tree
            | _ -> tree
            )
        //[5] Exp -> Exp power Exp 
        (fun tree ftab ->
            match tree with
            | (Result r)::(Result l)::tree ->
                (Result (System.Math.Pow(l,r)))::tree
            | _ -> tree
            )
        //[6] Exp -> Call 
        (fun tree ftab -> tree)
        //[7] Exp -> Atom 
        (fun tree ftab -> tree)
        //[8] Exp -> minus Exp 
        (fun tree ftab ->
            match tree with
            | (Result r)::tree ->
                (Result (-r))::tree
            | _ -> tree
            )
        //[9] Call -> id lpar Args rpar 
        (fun tree (ftab : Dictionary<string,int[] * System.Func<double[],double>>) ->
            //let (args,newTree) = args2arr tree
            match tree with
            | (CallArgs args)::(FuncId fid)::tree when ftab.ContainsKey(fid) ->
                let (argNumberA,f) = ftab.[fid]
                let argsGiven = List.length args
                if Array.exists (fun x -> x = argsGiven) argNumberA then
                    let argsA = Array.init argsGiven (fun i -> args.[i])
                    try (Result (f.Invoke(argsA)))::tree with
                    | _ -> failwith (sprintf "overflow in function %s" fid)
                else
                    failwith (sprintf "not the right amount of args given to %s" fid)
            | _::(FuncId fid)::tree ->
                let fmsg = sprintf "function '%s' does not exist" fid
                failwith fmsg
            | _ -> tree
            )
        //[10] Args -> 
        (fun tree ftab -> (CallArgs [])::tree)
        //[11] Args -> Arg Args' 
        (fun tree ftab ->
            match tree with
            | (CallArgs args)::(ArgResult r)::tree ->
                (CallArgs (r::args))::tree
            | _ -> tree
            )
        //[12] Args' -> 
        (fun tree ftab -> (CallArgs [])::tree)
        //[13] Args' -> comma Arg Args' 
        (fun tree ftab ->
            match tree with
            | (CallArgs args)::(ArgResult r)::tree ->
                (CallArgs (r::args))::tree
            | _ -> tree
            )
        //[14] Arg -> Exp 
        (fun tree ftab ->
            match tree with
            | (Result r)::tree ->
                (ArgResult r)::tree
            | _ -> tree
            )
        //[15] Atom -> lpar Exp rpar 
        (fun tree ftab -> tree)
        //[16] Atom -> float 
        (fun tree ftab ->
            match tree with
            | (ConstResult c)::tree ->
                (Result c)::tree
            | _ -> tree
            )
        //[17] Atom -> int 
        (fun tree ftab ->
            match tree with
            | (ConstResult c)::tree ->
                (Result c)::tree
            | _ -> tree
            )
        |]
    let lexer inStr =
        let tokensL = new List<string * Option<string> * (int * int)>()
        let lineLens = new Stack<int>()
        lineLens.Push(0)
        let addToken xIndex (tGroup : GroupCollection) =
            let totalLineLen = lineLens.Peek()
            if tGroup.[1].Value <> "" then
                tokensL.Add(("comma",None,(xIndex + 1 - totalLineLen,lineLens.Count)))
            if tGroup.[2].Value <> "" then
                tokensL.Add(("divide",None,(xIndex + 1 - totalLineLen,lineLens.Count)))
            if tGroup.[3].Value <> "" then
                tokensL.Add(("float",Some (tGroup.[3].Value),(xIndex + 1 - totalLineLen,lineLens.Count)))
            if tGroup.[4].Value <> "" then
                tokensL.Add(("id",Some (tGroup.[4].Value),(xIndex + 1 - totalLineLen,lineLens.Count)))
            if tGroup.[5].Value <> "" then
                tokensL.Add(("int",Some (tGroup.[5].Value),(xIndex + 1 - totalLineLen,lineLens.Count)))
            if tGroup.[6].Value <> "" then
                tokensL.Add(("lpar",None,(xIndex + 1 - totalLineLen,lineLens.Count)))
            if tGroup.[7].Value <> "" then
                tokensL.Add(("minus",None,(xIndex + 1 - totalLineLen,lineLens.Count)))
            if tGroup.[8].Value <> "" then
                tokensL.Add(("plus",None,(xIndex + 1 - totalLineLen,lineLens.Count)))
            if tGroup.[9].Value <> "" then
                tokensL.Add(("power",None,(xIndex + 1 - totalLineLen,lineLens.Count)))
            if tGroup.[10].Value <> "" then
                tokensL.Add(("rpar",None,(xIndex + 1 - totalLineLen,lineLens.Count)))
            if tGroup.[11].Value <> "" then
                tokensL.Add(("times",None,(xIndex + 1 - totalLineLen,lineLens.Count)))
        let regToken =
            "(,)|"+
            "(\\/)|"+
            "([0-9]*\\.[0-9]+)|"+
            "([a-zA-Z_]+)|"+
            "([0-9]+)|"+
            "(\\()|"+
            "(-)|"+
            "(\\+)|"+
            "(\\^)|"+
            "(\\))|"+
            "(\\*)|"+
            "\n|"+
            "\r|"+
            "\t|"+
            " ";
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
        (fun (dict : Dictionary<string,Action>) ->
            dict.Add("comma",Error "expected 'operand','parentheses','operator', but given 'comma'")
            dict.Add("divide",Error "expected 'operand','parentheses','operator', but given 'operator'")
            dict.Add("float",Shift 4)
            dict.Add("id",Shift 5)
            dict.Add("int",Shift 6)
            dict.Add("lpar",Shift 7)
            dict.Add("minus",Shift 8)
            dict.Add("plus",Error "expected 'operand','parentheses','operator', but given 'operator'")
            dict.Add("power",Error "expected 'operand','parentheses','operator', but given 'power'")
            dict.Add("rpar",Error "expected 'operand','parentheses','operator', but given 'parentheses'")
            dict.Add("times",Error "expected 'operand','parentheses','operator', but given 'times'")
            dict.Add("$",Error "expected 'operand','parentheses','operator', but given '$'")
            dict
            )(new Dictionary<string,Action>())
        (fun (dict : Dictionary<string,Action>) ->
            dict.Add("comma",Reduce 7)
            dict.Add("divide",Reduce 7)
            dict.Add("float",Error "expected 'comma','operator','power','parentheses','times','eoi', but given 'operand'")
            dict.Add("id",Error "expected 'comma','operator','power','parentheses','times','eoi', but given 'operand'")
            dict.Add("int",Error "expected 'comma','operator','power','parentheses','times','eoi', but given 'operand'")
            dict.Add("lpar",Error "expected 'comma','operator','power','parentheses','times','eoi', but given 'parentheses'")
            dict.Add("minus",Reduce 7)
            dict.Add("plus",Reduce 7)
            dict.Add("power",Reduce 7)
            dict.Add("rpar",Reduce 7)
            dict.Add("times",Reduce 7)
            dict.Add("$",Reduce 7)
            dict
            )(new Dictionary<string,Action>())
        (fun (dict : Dictionary<string,Action>) ->
            dict.Add("comma",Reduce 6)
            dict.Add("divide",Reduce 6)
            dict.Add("float",Error "expected 'comma','operator','power','parentheses','times','eoi', but given 'operand'")
            dict.Add("id",Error "expected 'comma','operator','power','parentheses','times','eoi', but given 'operand'")
            dict.Add("int",Error "expected 'comma','operator','power','parentheses','times','eoi', but given 'operand'")
            dict.Add("lpar",Error "expected 'comma','operator','power','parentheses','times','eoi', but given 'parentheses'")
            dict.Add("minus",Reduce 6)
            dict.Add("plus",Reduce 6)
            dict.Add("power",Reduce 6)
            dict.Add("rpar",Reduce 6)
            dict.Add("times",Reduce 6)
            dict.Add("$",Reduce 6)
            dict
            )(new Dictionary<string,Action>())
        (fun (dict : Dictionary<string,Action>) ->
            dict.Add("comma",Error "expected 'operator','power','times','eoi', but given 'comma'")
            dict.Add("divide",Shift 9)
            dict.Add("float",Error "expected 'operator','power','times','eoi', but given 'operand'")
            dict.Add("id",Error "expected 'operator','power','times','eoi', but given 'operand'")
            dict.Add("int",Error "expected 'operator','power','times','eoi', but given 'operand'")
            dict.Add("lpar",Error "expected 'operator','power','times','eoi', but given 'parentheses'")
            dict.Add("minus",Shift 10)
            dict.Add("plus",Shift 11)
            dict.Add("power",Shift 12)
            dict.Add("rpar",Error "expected 'operator','power','times','eoi', but given 'parentheses'")
            dict.Add("times",Shift 13)
            dict.Add("$",Accept)
            dict
            )(new Dictionary<string,Action>())
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
        (fun (dict : Dictionary<string,Action>) ->
            dict.Add("comma",Error "expected 'parentheses', but given 'comma'")
            dict.Add("divide",Error "expected 'parentheses', but given 'operator'")
            dict.Add("float",Error "expected 'parentheses', but given 'operand'")
            dict.Add("id",Error "expected 'parentheses', but given 'operand'")
            dict.Add("int",Error "expected 'parentheses', but given 'operand'")
            dict.Add("lpar",Shift 14)
            dict.Add("minus",Error "expected 'parentheses', but given 'operator'")
            dict.Add("plus",Error "expected 'parentheses', but given 'operator'")
            dict.Add("power",Error "expected 'parentheses', but given 'power'")
            dict.Add("rpar",Error "expected 'parentheses', but given 'parentheses'")
            dict.Add("times",Error "expected 'parentheses', but given 'times'")
            dict.Add("$",Error "expected 'parentheses', but given '$'")
            dict
            )(new Dictionary<string,Action>())
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
        (fun (dict : Dictionary<string,Action>) ->
            dict.Add("comma",Error "expected 'operand','parentheses','operator', but given 'comma'")
            dict.Add("divide",Error "expected 'operand','parentheses','operator', but given 'operator'")
            dict.Add("float",Shift 4)
            dict.Add("id",Shift 5)
            dict.Add("int",Shift 6)
            dict.Add("lpar",Shift 7)
            dict.Add("minus",Shift 8)
            dict.Add("plus",Error "expected 'operand','parentheses','operator', but given 'operator'")
            dict.Add("power",Error "expected 'operand','parentheses','operator', but given 'power'")
            dict.Add("rpar",Error "expected 'operand','parentheses','operator', but given 'parentheses'")
            dict.Add("times",Error "expected 'operand','parentheses','operator', but given 'times'")
            dict.Add("$",Error "expected 'operand','parentheses','operator', but given '$'")
            dict
            )(new Dictionary<string,Action>())
        (fun (dict : Dictionary<string,Action>) ->
            dict.Add("comma",Error "expected 'operand','parentheses','operator', but given 'comma'")
            dict.Add("divide",Error "expected 'operand','parentheses','operator', but given 'operator'")
            dict.Add("float",Shift 4)
            dict.Add("id",Shift 5)
            dict.Add("int",Shift 6)
            dict.Add("lpar",Shift 7)
            dict.Add("minus",Shift 8)
            dict.Add("plus",Error "expected 'operand','parentheses','operator', but given 'operator'")
            dict.Add("power",Error "expected 'operand','parentheses','operator', but given 'power'")
            dict.Add("rpar",Error "expected 'operand','parentheses','operator', but given 'parentheses'")
            dict.Add("times",Error "expected 'operand','parentheses','operator', but given 'times'")
            dict.Add("$",Error "expected 'operand','parentheses','operator', but given '$'")
            dict
            )(new Dictionary<string,Action>())
        (fun (dict : Dictionary<string,Action>) ->
            dict.Add("comma",Error "expected 'operand','parentheses','operator', but given 'comma'")
            dict.Add("divide",Error "expected 'operand','parentheses','operator', but given 'operator'")
            dict.Add("float",Shift 4)
            dict.Add("id",Shift 5)
            dict.Add("int",Shift 6)
            dict.Add("lpar",Shift 7)
            dict.Add("minus",Shift 8)
            dict.Add("plus",Error "expected 'operand','parentheses','operator', but given 'operator'")
            dict.Add("power",Error "expected 'operand','parentheses','operator', but given 'power'")
            dict.Add("rpar",Error "expected 'operand','parentheses','operator', but given 'parentheses'")
            dict.Add("times",Error "expected 'operand','parentheses','operator', but given 'times'")
            dict.Add("$",Error "expected 'operand','parentheses','operator', but given '$'")
            dict
            )(new Dictionary<string,Action>())
        (fun (dict : Dictionary<string,Action>) ->
            dict.Add("comma",Error "expected 'operand','parentheses','operator', but given 'comma'")
            dict.Add("divide",Error "expected 'operand','parentheses','operator', but given 'operator'")
            dict.Add("float",Shift 4)
            dict.Add("id",Shift 5)
            dict.Add("int",Shift 6)
            dict.Add("lpar",Shift 7)
            dict.Add("minus",Shift 8)
            dict.Add("plus",Error "expected 'operand','parentheses','operator', but given 'operator'")
            dict.Add("power",Error "expected 'operand','parentheses','operator', but given 'power'")
            dict.Add("rpar",Error "expected 'operand','parentheses','operator', but given 'parentheses'")
            dict.Add("times",Error "expected 'operand','parentheses','operator', but given 'times'")
            dict.Add("$",Error "expected 'operand','parentheses','operator', but given '$'")
            dict
            )(new Dictionary<string,Action>())
        (fun (dict : Dictionary<string,Action>) ->
            dict.Add("comma",Error "expected 'operand','parentheses','operator', but given 'comma'")
            dict.Add("divide",Error "expected 'operand','parentheses','operator', but given 'operator'")
            dict.Add("float",Shift 4)
            dict.Add("id",Shift 5)
            dict.Add("int",Shift 6)
            dict.Add("lpar",Shift 7)
            dict.Add("minus",Shift 8)
            dict.Add("plus",Error "expected 'operand','parentheses','operator', but given 'operator'")
            dict.Add("power",Error "expected 'operand','parentheses','operator', but given 'power'")
            dict.Add("rpar",Error "expected 'operand','parentheses','operator', but given 'parentheses'")
            dict.Add("times",Error "expected 'operand','parentheses','operator', but given 'times'")
            dict.Add("$",Error "expected 'operand','parentheses','operator', but given '$'")
            dict
            )(new Dictionary<string,Action>())
        (fun (dict : Dictionary<string,Action>) ->
            dict.Add("comma",Error "expected 'operand','parentheses','operator', but given 'comma'")
            dict.Add("divide",Error "expected 'operand','parentheses','operator', but given 'operator'")
            dict.Add("float",Shift 4)
            dict.Add("id",Shift 5)
            dict.Add("int",Shift 6)
            dict.Add("lpar",Shift 7)
            dict.Add("minus",Shift 8)
            dict.Add("plus",Error "expected 'operand','parentheses','operator', but given 'operator'")
            dict.Add("power",Error "expected 'operand','parentheses','operator', but given 'power'")
            dict.Add("rpar",Error "expected 'operand','parentheses','operator', but given 'parentheses'")
            dict.Add("times",Error "expected 'operand','parentheses','operator', but given 'times'")
            dict.Add("$",Error "expected 'operand','parentheses','operator', but given '$'")
            dict
            )(new Dictionary<string,Action>())
        (fun (dict : Dictionary<string,Action>) ->
            dict.Add("comma",Error "expected 'operand','parentheses','operator', but given 'comma'")
            dict.Add("divide",Error "expected 'operand','parentheses','operator', but given 'operator'")
            dict.Add("float",Shift 4)
            dict.Add("id",Shift 5)
            dict.Add("int",Shift 6)
            dict.Add("lpar",Shift 7)
            dict.Add("minus",Shift 8)
            dict.Add("plus",Error "expected 'operand','parentheses','operator', but given 'operator'")
            dict.Add("power",Error "expected 'operand','parentheses','operator', but given 'power'")
            dict.Add("rpar",Error "expected 'operand','parentheses','operator', but given 'parentheses'")
            dict.Add("times",Error "expected 'operand','parentheses','operator', but given 'times'")
            dict.Add("$",Error "expected 'operand','parentheses','operator', but given '$'")
            dict
            )(new Dictionary<string,Action>())
        (fun (dict : Dictionary<string,Action>) ->
            dict.Add("comma",Error "expected 'operand','parentheses','operator', but given 'comma'")
            dict.Add("divide",Error "expected 'operand','parentheses','operator', but given 'operator'")
            dict.Add("float",Shift 4)
            dict.Add("id",Shift 5)
            dict.Add("int",Shift 6)
            dict.Add("lpar",Shift 7)
            dict.Add("minus",Shift 8)
            dict.Add("plus",Error "expected 'operand','parentheses','operator', but given 'operator'")
            dict.Add("power",Error "expected 'operand','parentheses','operator', but given 'power'")
            dict.Add("rpar",Reduce 10)
            dict.Add("times",Error "expected 'operand','parentheses','operator', but given 'times'")
            dict.Add("$",Error "expected 'operand','parentheses','operator', but given '$'")
            dict
            )(new Dictionary<string,Action>())
        (fun (dict : Dictionary<string,Action>) ->
            dict.Add("comma",Error "expected 'operator','power','parentheses','times', but given 'comma'")
            dict.Add("divide",Shift 9)
            dict.Add("float",Error "expected 'operator','power','parentheses','times', but given 'operand'")
            dict.Add("id",Error "expected 'operator','power','parentheses','times', but given 'operand'")
            dict.Add("int",Error "expected 'operator','power','parentheses','times', but given 'operand'")
            dict.Add("lpar",Error "expected 'operator','power','parentheses','times', but given 'parentheses'")
            dict.Add("minus",Shift 10)
            dict.Add("plus",Shift 11)
            dict.Add("power",Shift 12)
            dict.Add("rpar",Shift 25)
            dict.Add("times",Shift 13)
            dict.Add("$",Error "expected 'operator','power','parentheses','times', but given '$'")
            dict
            )(new Dictionary<string,Action>())
        (fun (dict : Dictionary<string,Action>) ->
            dict.Add("comma",Reduce 8)
            dict.Add("divide",Shift 9)
            dict.Add("float",Error "expected 'comma','operator','power','parentheses','times','eoi', but given 'operand'")
            dict.Add("id",Error "expected 'comma','operator','power','parentheses','times','eoi', but given 'operand'")
            dict.Add("int",Error "expected 'comma','operator','power','parentheses','times','eoi', but given 'operand'")
            dict.Add("lpar",Error "expected 'comma','operator','power','parentheses','times','eoi', but given 'parentheses'")
            dict.Add("minus",Reduce 8)
            dict.Add("plus",Reduce 8)
            dict.Add("power",Shift 12)
            dict.Add("rpar",Reduce 8)
            dict.Add("times",Shift 13)
            dict.Add("$",Reduce 8)
            dict
            )(new Dictionary<string,Action>())
        (fun (dict : Dictionary<string,Action>) ->
            dict.Add("comma",Reduce 4)
            dict.Add("divide",Reduce 4)
            dict.Add("float",Error "expected 'comma','operator','power','parentheses','times','eoi', but given 'operand'")
            dict.Add("id",Error "expected 'comma','operator','power','parentheses','times','eoi', but given 'operand'")
            dict.Add("int",Error "expected 'comma','operator','power','parentheses','times','eoi', but given 'operand'")
            dict.Add("lpar",Error "expected 'comma','operator','power','parentheses','times','eoi', but given 'parentheses'")
            dict.Add("minus",Reduce 4)
            dict.Add("plus",Reduce 4)
            dict.Add("power",Shift 12)
            dict.Add("rpar",Reduce 4)
            dict.Add("times",Reduce 4)
            dict.Add("$",Reduce 4)
            dict
            )(new Dictionary<string,Action>())
        (fun (dict : Dictionary<string,Action>) ->
            dict.Add("comma",Reduce 1)
            dict.Add("divide",Shift 9)
            dict.Add("float",Error "expected 'comma','operator','power','parentheses','times','eoi', but given 'operand'")
            dict.Add("id",Error "expected 'comma','operator','power','parentheses','times','eoi', but given 'operand'")
            dict.Add("int",Error "expected 'comma','operator','power','parentheses','times','eoi', but given 'operand'")
            dict.Add("lpar",Error "expected 'comma','operator','power','parentheses','times','eoi', but given 'parentheses'")
            dict.Add("minus",Reduce 1)
            dict.Add("plus",Reduce 1)
            dict.Add("power",Shift 12)
            dict.Add("rpar",Reduce 1)
            dict.Add("times",Shift 13)
            dict.Add("$",Reduce 1)
            dict
            )(new Dictionary<string,Action>())
        (fun (dict : Dictionary<string,Action>) ->
            dict.Add("comma",Reduce 2)
            dict.Add("divide",Shift 9)
            dict.Add("float",Error "expected 'comma','operator','power','parentheses','times','eoi', but given 'operand'")
            dict.Add("id",Error "expected 'comma','operator','power','parentheses','times','eoi', but given 'operand'")
            dict.Add("int",Error "expected 'comma','operator','power','parentheses','times','eoi', but given 'operand'")
            dict.Add("lpar",Error "expected 'comma','operator','power','parentheses','times','eoi', but given 'parentheses'")
            dict.Add("minus",Reduce 2)
            dict.Add("plus",Reduce 2)
            dict.Add("power",Shift 12)
            dict.Add("rpar",Reduce 2)
            dict.Add("times",Shift 13)
            dict.Add("$",Reduce 2)
            dict
            )(new Dictionary<string,Action>())
        (fun (dict : Dictionary<string,Action>) ->
            dict.Add("comma",Reduce 5)
            dict.Add("divide",Reduce 5)
            dict.Add("float",Error "expected 'comma','operator','power','parentheses','times','eoi', but given 'operand'")
            dict.Add("id",Error "expected 'comma','operator','power','parentheses','times','eoi', but given 'operand'")
            dict.Add("int",Error "expected 'comma','operator','power','parentheses','times','eoi', but given 'operand'")
            dict.Add("lpar",Error "expected 'comma','operator','power','parentheses','times','eoi', but given 'parentheses'")
            dict.Add("minus",Reduce 5)
            dict.Add("plus",Reduce 5)
            dict.Add("power",Shift 12)
            dict.Add("rpar",Reduce 5)
            dict.Add("times",Reduce 5)
            dict.Add("$",Reduce 5)
            dict
            )(new Dictionary<string,Action>())
        (fun (dict : Dictionary<string,Action>) ->
            dict.Add("comma",Reduce 3)
            dict.Add("divide",Reduce 3)
            dict.Add("float",Error "expected 'comma','operator','power','parentheses','times','eoi', but given 'operand'")
            dict.Add("id",Error "expected 'comma','operator','power','parentheses','times','eoi', but given 'operand'")
            dict.Add("int",Error "expected 'comma','operator','power','parentheses','times','eoi', but given 'operand'")
            dict.Add("lpar",Error "expected 'comma','operator','power','parentheses','times','eoi', but given 'parentheses'")
            dict.Add("minus",Reduce 3)
            dict.Add("plus",Reduce 3)
            dict.Add("power",Shift 12)
            dict.Add("rpar",Reduce 3)
            dict.Add("times",Reduce 3)
            dict.Add("$",Reduce 3)
            dict
            )(new Dictionary<string,Action>())
        (fun (dict : Dictionary<string,Action>) ->
            dict.Add("comma",Shift 27)
            dict.Add("divide",Error "expected 'comma','parentheses', but given 'operator'")
            dict.Add("float",Error "expected 'comma','parentheses', but given 'operand'")
            dict.Add("id",Error "expected 'comma','parentheses', but given 'operand'")
            dict.Add("int",Error "expected 'comma','parentheses', but given 'operand'")
            dict.Add("lpar",Error "expected 'comma','parentheses', but given 'parentheses'")
            dict.Add("minus",Error "expected 'comma','parentheses', but given 'operator'")
            dict.Add("plus",Error "expected 'comma','parentheses', but given 'operator'")
            dict.Add("power",Error "expected 'comma','parentheses', but given 'power'")
            dict.Add("rpar",Reduce 12)
            dict.Add("times",Error "expected 'comma','parentheses', but given 'times'")
            dict.Add("$",Error "expected 'comma','parentheses', but given '$'")
            dict
            )(new Dictionary<string,Action>())
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
            dict.Add("rpar",Shift 28)
            dict.Add("times",Error "expected 'parentheses', but given 'times'")
            dict.Add("$",Error "expected 'parentheses', but given '$'")
            dict
            )(new Dictionary<string,Action>())
        (fun (dict : Dictionary<string,Action>) ->
            dict.Add("comma",Reduce 14)
            dict.Add("divide",Shift 9)
            dict.Add("float",Error "expected 'comma','operator','power','parentheses','times', but given 'operand'")
            dict.Add("id",Error "expected 'comma','operator','power','parentheses','times', but given 'operand'")
            dict.Add("int",Error "expected 'comma','operator','power','parentheses','times', but given 'operand'")
            dict.Add("lpar",Error "expected 'comma','operator','power','parentheses','times', but given 'parentheses'")
            dict.Add("minus",Shift 10)
            dict.Add("plus",Shift 11)
            dict.Add("power",Shift 12)
            dict.Add("rpar",Reduce 14)
            dict.Add("times",Shift 13)
            dict.Add("$",Error "expected 'comma','operator','power','parentheses','times', but given '$'")
            dict
            )(new Dictionary<string,Action>())
        (fun (dict : Dictionary<string,Action>) ->
            dict.Add("comma",Reduce 15)
            dict.Add("divide",Reduce 15)
            dict.Add("float",Error "expected 'comma','operator','power','parentheses','times','eoi', but given 'operand'")
            dict.Add("id",Error "expected 'comma','operator','power','parentheses','times','eoi', but given 'operand'")
            dict.Add("int",Error "expected 'comma','operator','power','parentheses','times','eoi', but given 'operand'")
            dict.Add("lpar",Error "expected 'comma','operator','power','parentheses','times','eoi', but given 'parentheses'")
            dict.Add("minus",Reduce 15)
            dict.Add("plus",Reduce 15)
            dict.Add("power",Reduce 15)
            dict.Add("rpar",Reduce 15)
            dict.Add("times",Reduce 15)
            dict.Add("$",Reduce 15)
            dict
            )(new Dictionary<string,Action>())
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
            dict.Add("rpar",Reduce 11)
            dict.Add("times",Error "expected 'parentheses', but given 'times'")
            dict.Add("$",Error "expected 'parentheses', but given '$'")
            dict
            )(new Dictionary<string,Action>())
        (fun (dict : Dictionary<string,Action>) ->
            dict.Add("comma",Error "expected 'operand','parentheses','operator', but given 'comma'")
            dict.Add("divide",Error "expected 'operand','parentheses','operator', but given 'operator'")
            dict.Add("float",Shift 4)
            dict.Add("id",Shift 5)
            dict.Add("int",Shift 6)
            dict.Add("lpar",Shift 7)
            dict.Add("minus",Shift 8)
            dict.Add("plus",Error "expected 'operand','parentheses','operator', but given 'operator'")
            dict.Add("power",Error "expected 'operand','parentheses','operator', but given 'power'")
            dict.Add("rpar",Error "expected 'operand','parentheses','operator', but given 'parentheses'")
            dict.Add("times",Error "expected 'operand','parentheses','operator', but given 'times'")
            dict.Add("$",Error "expected 'operand','parentheses','operator', but given '$'")
            dict
            )(new Dictionary<string,Action>())
        (fun (dict : Dictionary<string,Action>) ->
            dict.Add("comma",Reduce 9)
            dict.Add("divide",Reduce 9)
            dict.Add("float",Error "expected 'comma','operator','power','parentheses','times','eoi', but given 'operand'")
            dict.Add("id",Error "expected 'comma','operator','power','parentheses','times','eoi', but given 'operand'")
            dict.Add("int",Error "expected 'comma','operator','power','parentheses','times','eoi', but given 'operand'")
            dict.Add("lpar",Error "expected 'comma','operator','power','parentheses','times','eoi', but given 'parentheses'")
            dict.Add("minus",Reduce 9)
            dict.Add("plus",Reduce 9)
            dict.Add("power",Reduce 9)
            dict.Add("rpar",Reduce 9)
            dict.Add("times",Reduce 9)
            dict.Add("$",Reduce 9)
            dict
            )(new Dictionary<string,Action>())
        (fun (dict : Dictionary<string,Action>) ->
            dict.Add("comma",Shift 27)
            dict.Add("divide",Error "expected 'comma','parentheses', but given 'operator'")
            dict.Add("float",Error "expected 'comma','parentheses', but given 'operand'")
            dict.Add("id",Error "expected 'comma','parentheses', but given 'operand'")
            dict.Add("int",Error "expected 'comma','parentheses', but given 'operand'")
            dict.Add("lpar",Error "expected 'comma','parentheses', but given 'parentheses'")
            dict.Add("minus",Error "expected 'comma','parentheses', but given 'operator'")
            dict.Add("plus",Error "expected 'comma','parentheses', but given 'operator'")
            dict.Add("power",Error "expected 'comma','parentheses', but given 'power'")
            dict.Add("rpar",Reduce 12)
            dict.Add("times",Error "expected 'comma','parentheses', but given 'times'")
            dict.Add("$",Error "expected 'comma','parentheses', but given '$'")
            dict
            )(new Dictionary<string,Action>())
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
            dict.Add("rpar",Reduce 13)
            dict.Add("times",Error "expected 'parentheses', but given 'times'")
            dict.Add("$",Error "expected 'parentheses', but given '$'")
            dict
            )(new Dictionary<string,Action>())
        |]
    let gotoTable = [|
        (fun (dict : Dictionary<string,Option<int>>) ->
            dict.Add("Exp",Some 3)
            dict.Add("Call",Some 2)
            dict.Add("Args",None)
            dict.Add("Args'",None)
            dict.Add("Arg",None)
            dict.Add("Atom",Some 1)
            dict
            )(new Dictionary<string,Option<int>>())
        (fun (dict : Dictionary<string,Option<int>>) ->
            dict.Add("Exp",None)
            dict.Add("Call",None)
            dict.Add("Args",None)
            dict.Add("Args'",None)
            dict.Add("Arg",None)
            dict.Add("Atom",None)
            dict
            )(new Dictionary<string,Option<int>>())
        (fun (dict : Dictionary<string,Option<int>>) ->
            dict.Add("Exp",None)
            dict.Add("Call",None)
            dict.Add("Args",None)
            dict.Add("Args'",None)
            dict.Add("Arg",None)
            dict.Add("Atom",None)
            dict
            )(new Dictionary<string,Option<int>>())
        (fun (dict : Dictionary<string,Option<int>>) ->
            dict.Add("Exp",None)
            dict.Add("Call",None)
            dict.Add("Args",None)
            dict.Add("Args'",None)
            dict.Add("Arg",None)
            dict.Add("Atom",None)
            dict
            )(new Dictionary<string,Option<int>>())
        (fun (dict : Dictionary<string,Option<int>>) ->
            dict.Add("Exp",None)
            dict.Add("Call",None)
            dict.Add("Args",None)
            dict.Add("Args'",None)
            dict.Add("Arg",None)
            dict.Add("Atom",None)
            dict
            )(new Dictionary<string,Option<int>>())
        (fun (dict : Dictionary<string,Option<int>>) ->
            dict.Add("Exp",None)
            dict.Add("Call",None)
            dict.Add("Args",None)
            dict.Add("Args'",None)
            dict.Add("Arg",None)
            dict.Add("Atom",None)
            dict
            )(new Dictionary<string,Option<int>>())
        (fun (dict : Dictionary<string,Option<int>>) ->
            dict.Add("Exp",None)
            dict.Add("Call",None)
            dict.Add("Args",None)
            dict.Add("Args'",None)
            dict.Add("Arg",None)
            dict.Add("Atom",None)
            dict
            )(new Dictionary<string,Option<int>>())
        (fun (dict : Dictionary<string,Option<int>>) ->
            dict.Add("Exp",Some 15)
            dict.Add("Call",Some 2)
            dict.Add("Args",None)
            dict.Add("Args'",None)
            dict.Add("Arg",None)
            dict.Add("Atom",Some 1)
            dict
            )(new Dictionary<string,Option<int>>())
        (fun (dict : Dictionary<string,Option<int>>) ->
            dict.Add("Exp",Some 16)
            dict.Add("Call",Some 2)
            dict.Add("Args",None)
            dict.Add("Args'",None)
            dict.Add("Arg",None)
            dict.Add("Atom",Some 1)
            dict
            )(new Dictionary<string,Option<int>>())
        (fun (dict : Dictionary<string,Option<int>>) ->
            dict.Add("Exp",Some 17)
            dict.Add("Call",Some 2)
            dict.Add("Args",None)
            dict.Add("Args'",None)
            dict.Add("Arg",None)
            dict.Add("Atom",Some 1)
            dict
            )(new Dictionary<string,Option<int>>())
        (fun (dict : Dictionary<string,Option<int>>) ->
            dict.Add("Exp",Some 18)
            dict.Add("Call",Some 2)
            dict.Add("Args",None)
            dict.Add("Args'",None)
            dict.Add("Arg",None)
            dict.Add("Atom",Some 1)
            dict
            )(new Dictionary<string,Option<int>>())
        (fun (dict : Dictionary<string,Option<int>>) ->
            dict.Add("Exp",Some 19)
            dict.Add("Call",Some 2)
            dict.Add("Args",None)
            dict.Add("Args'",None)
            dict.Add("Arg",None)
            dict.Add("Atom",Some 1)
            dict
            )(new Dictionary<string,Option<int>>())
        (fun (dict : Dictionary<string,Option<int>>) ->
            dict.Add("Exp",Some 20)
            dict.Add("Call",Some 2)
            dict.Add("Args",None)
            dict.Add("Args'",None)
            dict.Add("Arg",None)
            dict.Add("Atom",Some 1)
            dict
            )(new Dictionary<string,Option<int>>())
        (fun (dict : Dictionary<string,Option<int>>) ->
            dict.Add("Exp",Some 21)
            dict.Add("Call",Some 2)
            dict.Add("Args",None)
            dict.Add("Args'",None)
            dict.Add("Arg",None)
            dict.Add("Atom",Some 1)
            dict
            )(new Dictionary<string,Option<int>>())
        (fun (dict : Dictionary<string,Option<int>>) ->
            dict.Add("Exp",Some 24)
            dict.Add("Call",Some 2)
            dict.Add("Args",Some 23)
            dict.Add("Args'",None)
            dict.Add("Arg",Some 22)
            dict.Add("Atom",Some 1)
            dict
            )(new Dictionary<string,Option<int>>())
        (fun (dict : Dictionary<string,Option<int>>) ->
            dict.Add("Exp",None)
            dict.Add("Call",None)
            dict.Add("Args",None)
            dict.Add("Args'",None)
            dict.Add("Arg",None)
            dict.Add("Atom",None)
            dict
            )(new Dictionary<string,Option<int>>())
        (fun (dict : Dictionary<string,Option<int>>) ->
            dict.Add("Exp",None)
            dict.Add("Call",None)
            dict.Add("Args",None)
            dict.Add("Args'",None)
            dict.Add("Arg",None)
            dict.Add("Atom",None)
            dict
            )(new Dictionary<string,Option<int>>())
        (fun (dict : Dictionary<string,Option<int>>) ->
            dict.Add("Exp",None)
            dict.Add("Call",None)
            dict.Add("Args",None)
            dict.Add("Args'",None)
            dict.Add("Arg",None)
            dict.Add("Atom",None)
            dict
            )(new Dictionary<string,Option<int>>())
        (fun (dict : Dictionary<string,Option<int>>) ->
            dict.Add("Exp",None)
            dict.Add("Call",None)
            dict.Add("Args",None)
            dict.Add("Args'",None)
            dict.Add("Arg",None)
            dict.Add("Atom",None)
            dict
            )(new Dictionary<string,Option<int>>())
        (fun (dict : Dictionary<string,Option<int>>) ->
            dict.Add("Exp",None)
            dict.Add("Call",None)
            dict.Add("Args",None)
            dict.Add("Args'",None)
            dict.Add("Arg",None)
            dict.Add("Atom",None)
            dict
            )(new Dictionary<string,Option<int>>())
        (fun (dict : Dictionary<string,Option<int>>) ->
            dict.Add("Exp",None)
            dict.Add("Call",None)
            dict.Add("Args",None)
            dict.Add("Args'",None)
            dict.Add("Arg",None)
            dict.Add("Atom",None)
            dict
            )(new Dictionary<string,Option<int>>())
        (fun (dict : Dictionary<string,Option<int>>) ->
            dict.Add("Exp",None)
            dict.Add("Call",None)
            dict.Add("Args",None)
            dict.Add("Args'",None)
            dict.Add("Arg",None)
            dict.Add("Atom",None)
            dict
            )(new Dictionary<string,Option<int>>())
        (fun (dict : Dictionary<string,Option<int>>) ->
            dict.Add("Exp",None)
            dict.Add("Call",None)
            dict.Add("Args",None)
            dict.Add("Args'",Some 26)
            dict.Add("Arg",None)
            dict.Add("Atom",None)
            dict
            )(new Dictionary<string,Option<int>>())
        (fun (dict : Dictionary<string,Option<int>>) ->
            dict.Add("Exp",None)
            dict.Add("Call",None)
            dict.Add("Args",None)
            dict.Add("Args'",None)
            dict.Add("Arg",None)
            dict.Add("Atom",None)
            dict
            )(new Dictionary<string,Option<int>>())
        (fun (dict : Dictionary<string,Option<int>>) ->
            dict.Add("Exp",None)
            dict.Add("Call",None)
            dict.Add("Args",None)
            dict.Add("Args'",None)
            dict.Add("Arg",None)
            dict.Add("Atom",None)
            dict
            )(new Dictionary<string,Option<int>>())
        (fun (dict : Dictionary<string,Option<int>>) ->
            dict.Add("Exp",None)
            dict.Add("Call",None)
            dict.Add("Args",None)
            dict.Add("Args'",None)
            dict.Add("Arg",None)
            dict.Add("Atom",None)
            dict
            )(new Dictionary<string,Option<int>>())
        (fun (dict : Dictionary<string,Option<int>>) ->
            dict.Add("Exp",None)
            dict.Add("Call",None)
            dict.Add("Args",None)
            dict.Add("Args'",None)
            dict.Add("Arg",None)
            dict.Add("Atom",None)
            dict
            )(new Dictionary<string,Option<int>>())
        (fun (dict : Dictionary<string,Option<int>>) ->
            dict.Add("Exp",Some 24)
            dict.Add("Call",Some 2)
            dict.Add("Args",None)
            dict.Add("Args'",None)
            dict.Add("Arg",Some 29)
            dict.Add("Atom",Some 1)
            dict
            )(new Dictionary<string,Option<int>>())
        (fun (dict : Dictionary<string,Option<int>>) ->
            dict.Add("Exp",None)
            dict.Add("Call",None)
            dict.Add("Args",None)
            dict.Add("Args'",None)
            dict.Add("Arg",None)
            dict.Add("Atom",None)
            dict
            )(new Dictionary<string,Option<int>>())
        (fun (dict : Dictionary<string,Option<int>>) ->
            dict.Add("Exp",None)
            dict.Add("Call",None)
            dict.Add("Args",None)
            dict.Add("Args'",Some 30)
            dict.Add("Arg",None)
            dict.Add("Atom",None)
            dict
            )(new Dictionary<string,Option<int>>())
        (fun (dict : Dictionary<string,Option<int>>) ->
            dict.Add("Exp",None)
            dict.Add("Call",None)
            dict.Add("Args",None)
            dict.Add("Args'",None)
            dict.Add("Arg",None)
            dict.Add("Atom",None)
            dict
            )(new Dictionary<string,Option<int>>())
         |]
    let productions_str = [|
        ("__",[|"Exp"|])
        ("Exp",[|"Exp";"minus";"Exp"|])
        ("Exp",[|"Exp";"plus";"Exp"|])
        ("Exp",[|"Exp";"times";"Exp"|])
        ("Exp",[|"Exp";"divide";"Exp"|])
        ("Exp",[|"Exp";"power";"Exp"|])
        ("Exp",[|"Call"|])
        ("Exp",[|"Atom"|])
        ("Exp",[|"minus";"Exp"|])
        ("Call",[|"id";"lpar";"Args";"rpar"|])
        ("Args",[||])
        ("Args",[|"Arg";"Args'"|])
        ("Args'",[||])
        ("Args'",[|"comma";"Arg";"Args'"|])
        ("Arg",[|"Exp"|])
        ("Atom",[|"lpar";"Exp";"rpar"|])
        ("Atom",[|"float"|])
        ("Atom",[|"int"|])
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
            if i < tLen - 1 then
                (i + 1,tokens.[i+1])
            else failwith "parser: getNextFromInputError"
        let rec exec (i,a) sStack tree =
            let (s,_) = popOne sStack
            match actionTable.[s].[a|>(fun (a,_,_) -> a)] with
            | Shift t ->
                let newStack = t::sStack
                let newTree = addToken2tree tree a
                let (i,a) = getNextFromInput i
                exec (i,a) newStack newTree
            | Reduce r ->
                let (prod,rSide,prodF,pos) =
                    let (a0,b0) = productions_str.[r]
                    let f = productions_fun.[r]
                    let p = a|>(fun (_,_,pos) -> pos)
                    (a0,b0,f,p)
                let newTree = prodF tree ftab
                let betaLen = Array.length rSide
                let (_,newStack) = popN betaLen sStack
                let (t,_) = popOne newStack
                let newStack = pushGoto newStack gotoTable.[t].[prod]
                exec (i,a) newStack newTree
            | Accept -> tree
            | Error msg ->
                let (x,y) = a|>(fun (_,_,pos) -> pos)
                failwith (sprintf "syntax error(%d,%d)" y x)
        exec (0,tokens.[0]) [0] []
    let parse str (ftab : Dictionary<string,int[] * System.Func<float[],float>>) =
        let lexed = lexer str
        let parsed = parser lexed ftab
        parsed
