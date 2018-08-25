namespace Maskiner
module Prop2Table =
    open System.Collections.Generic
    open PropParser
    let bval = function
        | true -> "T"
        | _ -> "F"
    let twoPow n =
        let lookup = [|1;2;4;8;16;32;64;128;256;512;1024|]
        let lookupMax = (Array.length lookup) - 1 
        let rec exec acc n =
            if n = 0 then 1 
            elif n = 1 then acc
            else exec (acc * 2) (n - 1)
        if n < 0 then 0 //undefined behavior
        elif n <= lookupMax then lookup.[n] 
        else exec lookup.[lookupMax] (n + 1 - lookupMax)
    let echoAtom args (echoes : Dictionary<string,System.Func<string [],bool>>) =
        let fName = "atom"
        if echoes.ContainsKey(fName) then
            let echo = echoes.[fName].Invoke(args)
            ()
        else ()
    let echoExpSingle args (echoes : Dictionary<string,System.Func<string [],bool>>) =
        let fName = "exp_single"
        if echoes.ContainsKey(fName) then
            let echo = echoes.[fName].Invoke(args)
            ()
        else ()
    let echoExpDouble args (echoes : Dictionary<string,System.Func<string [],bool>>) =
        let fName = "exp_double"
        if echoes.ContainsKey(fName) then
            let echo = echoes.[fName].Invoke(args)
            ()
        else ()
    let echoModel args (echoes : Dictionary<string,System.Func<string [],bool>>) =
        let fName = "model"
        if echoes.ContainsKey(fName) then
            let echo = echoes.[fName].Invoke(args)
            ()
        else ()
    let permuteAtoms vtab =
        let n = List.length vtab
        let colLen = twoPow n
        let (_,retval) =
            List.fold
                (fun (alt,cols : Dictionary<string,bool []>) varName ->
                    let mutable tVal = false
                    let col () =
                        Array.init
                            colLen
                            (fun i ->
                                if tVal && i % alt = 0 then
                                    tVal <- false
                                    tVal
                                elif not tVal && i % alt = 0 then
                                    tVal <- true
                                    tVal
                                else tVal
                                )
                    let cols =
                        if not (cols.ContainsKey(varName)) then
                            cols.Add(varName,col())
                            cols
                        else cols
                    (alt * 2,cols)
                    )
                (1,new Dictionary<string,bool []>())
                vtab
        (colLen,retval)
    let rec evalTree (model : Dictionary<string,bool>) echoes = function
        | Leaf (Atom v) when v = "T" || v = "F" ->
            let res =
                if v = "T" then true
                else false
            let echo = echoAtom [|"const";res|>bval|] echoes
            res
        | Leaf (Atom p) when model.ContainsKey(p) ->
            let v = model.[p]
            let echo = echoAtom [|"var";p;v|>bval|] echoes
            v
        | NodeOne (Not,exp0) ->
            let v = evalTree model echoes exp0
            let res = not v
            let echo = echoExpSingle [|"not";v|>bval;res|>bval|] echoes
            res
        | NodeTwo (Imp,exp0,exp1) ->
            let v0 = evalTree model echoes exp0
            let v1 = evalTree model echoes exp1
            let res = (not v0) || v1
            let echo =
                echoExpDouble
                    [|"imp";v0|>bval;v1|>bval;res|>bval|]
                    echoes
            res
        | NodeTwo (Biimp,exp0,exp1) ->
            let v0 = evalTree model echoes exp0
            let v1 = evalTree model echoes exp1
            //let res = ((not v0) || v1) && ((not v1) || v0)
            let res = v0 = v1
            let echo =
                echoExpDouble
                    [|"biimp";v0|>bval;v1|>bval;res|>bval|]
                    echoes
            res
        | NodeTwo (Or,exp0,exp1) ->
            let v0 = evalTree model echoes exp0
            let v1 = evalTree model echoes exp1
            let res = v0 || v1
            let echo =
                echoExpDouble
                    [|"or";v0|>bval;v1|>bval;res|>bval|]
                    echoes
            res
        | NodeTwo (And,exp0,exp1) ->
            let v0 = evalTree model echoes exp0
            let v1 = evalTree model echoes exp1
            let res = v0 && v1
            let echo =
                echoExpDouble
                    [|"and";v0|>bval;v1|>bval;res|>bval|]
                    echoes
            res
        | e -> failwith (sprintf "unable to eval exp : %A" e)
    let initModel n (atoms : Dictionary<string,bool []>) =
        Dict<string,bool []>.fold
            (fun (newPerm : Dictionary<string,bool>) row ->
                let v = row.Key
                if not (newPerm.ContainsKey(v)) then
                    newPerm.Add(v,row.Value.[n])
                    newPerm
                else newPerm
                )
            (new Dictionary<string,bool>())
            atoms
    let eval vtab tree echoes =
        let (tLen,atoms) = permuteAtoms vtab
        let rec exec = function
            | n when n = tLen -> vtab|>List.toArray
            | n ->
                let model = initModel n atoms
                let evalModel = evalTree model echoes tree
                echoModel [|(string) n;evalModel|>bval|] echoes 
                exec (n + 1)
        exec 0
