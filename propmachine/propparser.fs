namespace Maskiner
module PropParser =
    open System.Text.RegularExpressions
    open System.Collections.Generic
    type Token =
        | Imp | Biimp | Or | And | Not
        | LPar | RPar
        | Atom of string
        | Dollar
    type Action =
        | Reduce of int
        | Shift of int
        | Error of string
        | Accept
    type Tree =
        | NodeTwo of Token * Tree * Tree
        | NodeOne of Token * Tree
        | Leaf of Token
        | InitLeaf of Token
        | EmptyTree
    (* The lexer part of the parser
     * *)
    let lexer inStr =
        let tokensL = new List<Token>()
        let addToken (tGroup : GroupCollection) =
            if tGroup.[1].Value <> "" then
                tokensL.Add(Imp)
            elif tGroup.[2].Value <> "" then
                tokensL.Add(Biimp)
            elif tGroup.[3].Value <> "" then
                tokensL.Add(Or)
            elif tGroup.[4].Value <> "" then
                tokensL.Add(And)
            elif tGroup.[5].Value <> "" then
                tokensL.Add(Not)
            elif tGroup.[6].Value <> "" then
                tokensL.Add(LPar)
            elif tGroup.[7].Value <> "" then
                tokensL.Add(RPar)
            elif tGroup.[8].Value <> "" then
                tokensL.Add(Atom tGroup.[8].Value)
            else ()
        let regToken =
            "(=>|->)|"+
            "(<=>|<->)|"+
            "(\\|\\||\\\\/)|"+
            "(&&|/\\\\)|"+
            "(~)|"+
            "(\\()|"+
            "(\\))|"+
            "([a-z]|T|F)|"+
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
    (* Grammar productions. These are used by the parsing alg.
     * *)
    let productions_str = [|
        ("Exp'",[|"Exp"|])
        ("Exp",[|"Exp";"imp";"Exp"|])
        ("Exp",[|"Exp";"biimp";"Exp"|])
        ("Exp",[|"Exp";"or";"Exp"|])
        ("Exp",[|"Exp";"and";"Exp"|])
        ("Exp",[|"not";"Exp"|])
        ("Exp",[|"Atom"|])
        ("Atom",[|"lpar";"Exp";"rpar"|])
        ("Atom",[|"atom"|])
        |]
    let productions_fun = [|
        //Exp' -> Exp
        (fun tree -> tree)
        //Exp -> Exp imp Exp
        (fun tree ->
            match tree with
            | nodeR::nodeL::tree ->
                (NodeTwo (Imp,nodeL,nodeR))::tree
            | _ -> tree
            )
        //Exp -> Exp biimp Exp
        (fun tree ->
            match tree with
            | nodeR::nodeL::tree ->
                (NodeTwo (Biimp,nodeL,nodeR))::tree
            | _ -> tree 
            )
        //Exp -> Exp or Exp
        (fun tree ->
            match tree with
            | nodeR::nodeL::tree ->
                (NodeTwo (Or,nodeL,nodeR))::tree
            | _ -> tree
            )
        //Exp -> Exp and Exp
        (fun tree ->
            match tree with
            | nodeR::nodeL::tree ->
                (NodeTwo (And,nodeL,nodeR))::tree
            | _ -> tree
            )
        //Exp -> not Exp
        (fun tree ->
            match tree with
            | node::tree ->
                (NodeOne (Not,node))::tree
            | _ -> tree
            )
        //Exp -> Atom
        (fun tree -> tree)
        //Atom -> lpar Exp rpar
        (fun tree -> tree)
        //Atom -> atom
        (fun tree ->
            match tree with
            | (InitLeaf t)::tree ->
                (Leaf t)::tree
            | _ -> tree
            )
        |]
    (* Construction of both action and goto-table 
     * *)
    let actionrow2dict row =
        let cName = [|Imp;Biimp;Or;And;Not;LPar;RPar;Atom "";Dollar|]
        let len = Array.length row
        let rec exec (d : Dictionary<Token,Action>) i =
            if i = len then d
            else
                d.Add(cName.[i],row.[i])
                exec d (i + 1)
        if len <> Array.length cName then failwith "internal error"
        else
            let d = new Dictionary<Token,Action>()
            exec d 0

    let gotorow2dict row =
        let cName = [|"Exp";"Atom"|]
        let len = Array.length row
        let rec exec (d : Dictionary<string,Option<int>>) i =
            if i = len then d
            else
                d.Add(cName.[i],row.[i])
                exec d (i + 1)
        if len <> Array.length cName then failwith "internal error"
        else
            let d = new Dictionary<string,Option<int>>()
            exec d 0

    let e0 = Error "some error"
    let e_rpar = Error "missing right parenthesis" //e1
    let e_exp_eoi = Error "expression expected but given end of input" //e2
    let e_exp_rpar = Error "expression expected but given )" //e3
    let e_exp_op = Error "expression expected but given operator" //e4
    let e_op_exp = Error "operator expected but given expression" //e5
    let actionTable = [|
        actionrow2dict [|e_exp_op;e_exp_op;e_exp_op;e_exp_op;Shift 5;Shift 4;e_exp_rpar;Shift 3;e_exp_eoi|] //0
        actionrow2dict [|Reduce 6;Reduce 6;Reduce 6;Reduce 6;e_op_exp;e_op_exp;Reduce 6;e_op_exp;Reduce 6|] //1
        actionrow2dict [|Shift 8;Shift 7;Shift 9;Shift 6;e_op_exp;e_op_exp;e_op_exp;e_op_exp;Accept|] //2
        actionrow2dict [|Reduce 8;Reduce 8;Reduce 8;Reduce 8;e_op_exp;e_op_exp;Reduce 8;e_op_exp;Reduce 8|] //3
        actionrow2dict [|e_exp_op;e_exp_op;e_exp_op;e_exp_op;Shift 5;Shift 4;e_exp_rpar;Shift 3;e_exp_eoi|] //4
        actionrow2dict [|e_exp_op;e_exp_op;e_exp_op;e_exp_op;Shift 5;Shift 4;e_exp_rpar;Shift 3;e_exp_eoi|] //5
        actionrow2dict [|e_exp_op;e_exp_op;e_exp_op;e_exp_op;Shift 5;Shift 4;e_exp_rpar;Shift 3;e_exp_eoi|] //6
        actionrow2dict [|e_exp_op;e_exp_op;e_exp_op;e_exp_op;Shift 5;Shift 4;e_exp_rpar;Shift 3;e_exp_eoi|] //7
        actionrow2dict [|e_exp_op;e_exp_op;e_exp_op;e_exp_op;Shift 5;Shift 4;e_exp_rpar;Shift 3;e_exp_eoi|] //8
        actionrow2dict [|e_exp_op;e_exp_op;e_exp_op;e_exp_op;Shift 5;Shift 4;e_exp_rpar;Shift 3;e_exp_eoi|] //9
        actionrow2dict [|Shift 8;Shift 7;Shift 9;Shift 6;e_op_exp;e_op_exp;Shift 16;e_op_exp;e_rpar|] //10
        actionrow2dict [|Reduce 5;Reduce 5;Reduce 5;Reduce 5;e_op_exp;e_op_exp;Reduce 5;e_op_exp;Reduce 5|] //11
        actionrow2dict [|Reduce 4;Reduce 4;Reduce 4;Reduce 4;e_op_exp;e_op_exp;Reduce 4;e_op_exp;Reduce 4|] //12
        actionrow2dict [|Shift 8;Shift 7;Shift 9;Shift 6;e_op_exp;e_op_exp;Reduce 2;e_op_exp;Reduce 2|] //13
        actionrow2dict [|Shift 8;Shift 7;Shift 9;Shift 6;e_op_exp;e_op_exp;Reduce 1;e_op_exp;Reduce 1|] //14
        actionrow2dict [|Reduce 3;Reduce 3;Reduce 3;Reduce 3;e_op_exp;e_op_exp;Reduce 3;e_op_exp;Reduce 3|] //15
        actionrow2dict [|Reduce 7;Reduce 7;Reduce 7;Reduce 7;e_op_exp;e_op_exp;Reduce 7;e_op_exp;Reduce 7|] //16
        |]
    let gotoTable = [|
        gotorow2dict [|Some 2;Some 1|]
        gotorow2dict [|None;None|]
        gotorow2dict [|None;None|]
        gotorow2dict [|None;None|]
        gotorow2dict [|Some 10;Some 1|]
        gotorow2dict [|Some 11;Some 1|]
        gotorow2dict [|Some 12;Some 1|]
        gotorow2dict [|Some 13;Some 1|]
        gotorow2dict [|Some 14;Some 1|]
        gotorow2dict [|Some 15;Some 1|]
        gotorow2dict [|None;None|]
        gotorow2dict [|None;None|]
        gotorow2dict [|None;None|]
        gotorow2dict [|None;None|]
        gotorow2dict [|None;None|]
        gotorow2dict [|None;None|]
        gotorow2dict [|None;None|]
        |]
    (* The parser working on a set of tokens
     * *)
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
            if i < tLen then
                (i + 1,tokens.[i+1])
            else failwith "parser: getNextFromInputError"
        let removeAtomName = function
            | Atom _ -> Atom ""
            | t -> t
        let addAtom2tree vtab tree node =
            let isConst = function
                | "T" | "F" -> true
                | _ -> false
            let add2vtab v =
                if v|>isConst || List.exists (fun x -> x = v) vtab then
                    vtab
                else v::vtab
            match (tree,node) with
            | ([],Atom t) ->
                (add2vtab t,[InitLeaf (Atom t)])
            | (_,Atom t) -> (add2vtab t,(InitLeaf (Atom t))::tree)
            | _ -> (vtab,tree)
        let rec exec (i,a) sStack vtab tree =
            let (s,_) = popOne sStack
            match actionTable.[s].[removeAtomName a] with
            | Shift t ->
                let newStack = t::sStack
                let (newVtab,newTree) = addAtom2tree vtab tree a
                let (i,a) = getNextFromInput i
                exec (i,a) newStack newVtab newTree
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
                exec (i,a) newStack vtab newTree
            | Accept -> (vtab,tree)
            | Error msg -> failwith (sprintf "syntax error: %s" msg)
        exec (0,tokens.[0]) [0] [] []
    let parse str =
        let lexed = lexer str
        let (vtab,parsed) = parser lexed
        if List.length parsed = 1 then
            (List.rev vtab,parsed.[0])
        else
            failwith "parsing error"
