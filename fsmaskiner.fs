namespace Maskiner
open System.Collections.Generic
open System
type Main =
    static member nfa2dfa (name,nfa,echo) =
        let nfa = nfa + "\n"
        let (msg0,parsed) =
            try ("",NfaParser.go nfa) with
            | Failure msg -> (msg,new Dictionary<string,Nfa2Dfa.State>())
        let (msg1,_) =
            if msg0 <> "" then (msg0,[||])  
            else
                try ("",Nfa2Dfa.convert name parsed echo) with
                | Failure msg -> (msg,[||])
        msg1
    static member grammar2set (inStr,echoes : Dictionary<string,Func<string [],bool>>) =
        let nullableName = "nullable"
        let firstName = "first"
        let followName = "follow"
        let errorName = "error"
        let emptyDict = new Dictionary<string,HashSet<Grammar2Set.ProdExp>>()
        let set2str set =
            let retstr =
                Set<Grammar2Set.ProdExp>.fold
                    (fun acc e ->
                        match e with
                        | Grammar2Set.Term t ->
                            let t =
                                if t = "{" || t = "}" || t = "," then
                                    "'" + t + "'"
                                else t
                            acc + "," + t
                        | Grammar2Set.Dollar -> acc + ",$"
                        | _ -> acc
                        )
                    ""
                    set
            if String.length retstr = 0 then "{}"
            else
                let retstr0 = retstr.[1..]
                "{" + retstr0 + "}"
        let printError alg msg =
            if echoes.ContainsKey(errorName) then
                echoes.[errorName].Invoke([|alg;msg|])
            else false
        let printNullable (nullables : Dictionary<string,bool>) pOrder =
            let printF = echoes.[nullableName]
            let bool2str b =
                if b then "true"
                else "false"
            List.fold
                (fun acc pName ->
                    let p = nullables.[pName]
                    printF.Invoke([|pName;bool2str p|])
                    )
                true
                pOrder
        let printFirst (first : Dictionary<string,HashSet<Grammar2Set.ProdExp>>) pOrder =
            let printF = echoes.[firstName]
            List.fold
                (fun acc pName ->
                    let p = first.[pName]
                    printF.Invoke([|pName;set2str p|])
                    )
                true
                pOrder
        let printFollow (follow : Dictionary<string,HashSet<Grammar2Set.ProdExp>>) pOrder =
            let printF = echoes.[followName]
            List.fold
                (fun acc pName ->
                    let p = follow.[pName]
                    printF.Invoke([|pName;set2str p|])
                    )
                true
                pOrder
        let execNullable grammar pOrder =
            let (set,emsg) =
                try (Grammar2Set.calcNullable grammar,"") with
                | Failure msg ->
                    (new Dictionary<string,bool>(),msg)
            if emsg <> "" then
                (printError nullableName emsg,set)
            elif echoes.ContainsKey(nullableName) then
                (printNullable set pOrder,set)
            else (true,set)
        let execFirst grammar nSet pOrder =
            let (set,msg) =
                try (Grammar2Set.calcFirst grammar nSet,"") with
                | Failure msg ->
                    (emptyDict,msg)
            if msg <> "" then
                (printError firstName msg,set)
            elif echoes.ContainsKey(firstName) then
                (printFirst set pOrder,set)
            else (true,set)
        let execFollow grammar fSet nSet pOrder =
            let (set,msg) =
                try (Grammar2Set.calcFollow grammar fSet nSet,"") with
                | Failure msg ->
                    (emptyDict,msg)
            if msg <> "" then
                (printError followName msg,set)
            elif echoes.ContainsKey(followName) then
                (printFollow set pOrder,set)
            else (true,set)
        let ((grammarTree,pOrder),emsg0) =
            try ((GrammarParser.parseGrammar inStr),"") with
            | Failure msg ->
                let emptyTree = new Dictionary<string,(Grammar2Set.ProdExp list) list>()
                ((emptyTree,[]),msg)
        if emsg0 <> "" then
            printError "parser" emsg0
        elif echoes.ContainsKey(followName) then
            let (successNullable,nSet) = execNullable grammarTree pOrder
            let (successFirst,fSet) =
                if successNullable then
                    execFirst grammarTree nSet pOrder 
                else (false,emptyDict)
            let (successFollow,_) =
                if successFirst then
                    execFollow grammarTree fSet nSet pOrder
                else (false,emptyDict)
            successFollow
        elif echoes.ContainsKey(firstName) then
            let (successNullable,nSet) =
                execNullable grammarTree pOrder
            let (successFirst,_) =
                execFirst grammarTree nSet pOrder
            successFirst
        elif echoes.ContainsKey(nullableName) then
            let (successNullable,_) =
                execNullable grammarTree pOrder
            successNullable
        else true
