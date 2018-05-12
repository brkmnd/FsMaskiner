namespace Maskiner
(*
 * Module NfaParser
 * Module NfaParser
 * Module NfaParser
 * Module NfaParser
 * Module NfaParser
 * *)
module NfaParser =
    open System.Text.RegularExpressions
    open System.Collections.Generic
    type Token =
        | Name of string
        | Start
        | LPar | RPar | DLPar | DRPar
        | Line | RightPointer
        | Epsilon
        | Newline
        | Dollar
    type Node =
        | State of bool * string
        | AcceptingState of bool * string
        | Transition of string * string * string
    type MatchError =
        | StateError of Token * Token * Token
        | FinalStateError of Token * Token * Token
        | TransitionError of Token * Token * Token
        | GeneralError of Token []
    (*
     * Set of tokens. These are converted
     * to the proper type constructor in
     * the lexer
     * *)
    let regToken =
        "start|"+
        "-|>|\\(\\(|\\)\\)|\\(|\\)|"+
        "[a-zA-Z0-9]+|"+
        "\\n+| "+
        "#[^\\n]+"
    (*
     * Lexer
     * Replaces every token with "" in input str.
     * If the resulting replaced string isn't empty
     * report error, else return an array of tokens.
     * *)
    let lexer str =
        let tokensL = new List<Token>()
        let addToken t =
            let l = String.length t
            if t = "start" then tokensL.Add(Start)
            elif t = "(" then
                tokensL.Add(LPar)
            elif t = "((" then
                tokensL.Add(DLPar)
            elif t = ")" then
                tokensL.Add(RPar)
            elif t = "))" then
                tokensL.Add(DRPar)
            elif t = ">" then
                tokensL.Add(RightPointer)
            elif t = "-" then
                tokensL.Add(Line)
                elif t = " " || t.[0] = '#' then ()
            elif l > 0 && t.[0] = '\n' then
                tokensL.Add(Newline)
            else
                tokensL.Add(Name t)
        let mfun (m : Match) =
            addToken m.Value
            ""
        let residueStr = Regex.Replace(str,regToken,mfun)
        if residueStr <> "" then
            failwith (sprintf "garbage in input : %c" residueStr.[0])
        else
            let addEnd = tokensL.Add(Dollar)
            Array.init tokensL.Count (fun i -> tokensL.[i])
    (*
     * Standard LL(1) parser. It is described in details on
     * brkmnd.com/pages/projects under "nfa2dfa"-converter.
     * ToDo : add tupled transitions ex. -a,b>
     * *)
    let parser tokens =
        let token2str = function
            | LPar -> "("
            | RPar -> ")"
            | DLPar -> "(("
            | DRPar -> "(("
            | Line -> "-"
            | RightPointer -> ">"
            | Start -> "start"
            | Newline -> "newline"
            | Name n -> n
            | _ -> "internal"
        let errorR input =
            failwith (sprintf "syntax error: '%s'" (token2str input))
        let errorM = function
            | StateError (a,b,c) ->
                let aStr = token2str a
                let bStr = token2str b
                let cStr = token2str c
                let msg =
                    sprintf
                        "syntax error in def state: '%s %s %s'"
                        aStr
                        bStr
                        cStr
                failwith msg
            | FinalStateError (a,b,c) ->
                let aStr = token2str a
                let bStr = token2str b
                let cStr = token2str c
                let msg =
                    sprintf
                        "syntax error in def accepting state: '%s %s %s'"
                        aStr
                        bStr
                        cStr
                failwith msg
            | TransitionError (a,b,c) ->
                    let aStr = token2str a
                    let bStr = token2str b
                    let cStr = token2str c
                    let msg =
                        sprintf
                            "syntax error in transition: '%s %s %s'"
                            aStr
                            bStr
                            cStr
                    failwith msg
            | GeneralError tRest ->
                let len = Array.length tRest
                if len > 2 then
                    let aStr = token2str tRest.[0]
                    let bStr = token2str tRest.[1]
                    let cStr = token2str tRest.[2]
                    let msg =
                        sprintf
                            "syntax error: '%s %s %s'" aStr bStr cStr
                    failwith msg
                elif len > 1 then
                    let aStr = token2str tRest.[0]
                    let bStr = token2str tRest.[1]
                    let msg = sprintf "syntax error: '%s %s'" aStr bStr
                    failwith msg
                else
                    let aStr = token2str tRest.[0]
                    let msg = sprintf "syntax error: '%s'" aStr
                    failwith msg
        let len = Array.length tokens
        let tree = new Dictionary<string,Nfa2Dfa.State>()
        let add2tree = function
            | State (startAtt,name) ->
                tree.Add(name,Nfa2Dfa.newState false startAtt [])
                ()
            | AcceptingState (startAtt,name) ->
                tree.Add(name,Nfa2Dfa.newState true startAtt [])
                ()
            | Transition (targetName,c,destName) ->
                if not (tree.ContainsKey(targetName)) then
                    let msg =
                        sprintf
                            "transition from undefined state (%s)"
                            targetName
                    failwith msg
                else
                    let s0 = tree.[targetName]
                    let s = s0.start
                    let acc = s0.accepting
                    let ts = s0.transitions
                    let t0 =
                        if c = "eps" then
                            Nfa2Dfa.Epsilon destName
                        else Nfa2Dfa.Char (c,destName)
                    let ns0 = Nfa2Dfa.newState acc s (t0::ts)
                    tree.[targetName] <- ns0
        let matchF i input =
            let mLen tl = len - i - tl > 0
            match input with
            | Start when mLen 1 ->
                (i+1,tokens.[i+1],"")
            | LPar when mLen 3 ->
                match (tokens.[i],tokens.[i+1],tokens.[i+2]) with
                | (LPar,Name stateName,RPar) ->
                    (i+3,tokens.[i+3],stateName)
                | tError -> errorM (StateError tError) 
            | DLPar when mLen 2 ->
                match (tokens.[i],tokens.[i+1],tokens.[i+2]) with
                | (DLPar,Name stateName,DRPar) ->
                    (i+3,tokens.[i+3],stateName)
                | tError -> errorM (FinalStateError tError)
            | Line when mLen 3 ->
                match (tokens.[i],tokens.[i+1],tokens.[i+2]) with
                | (Line,Name tChar,RightPointer) ->
                    (i+3,tokens.[i+3],tChar)
                | tError -> errorM (TransitionError tError)
            | Newline when mLen 1 ->
                (i+1,tokens.[i+1],"")
            | Dollar ->
                (len-1,tokens.[len-1],"")
            | Epsilon ->
                (i,input,"")
            | _ -> errorM (GeneralError tokens.[i..])
        let rec parseS i input startAtt =
            match input with
            | Start | LPar | DLPar ->
                let (i,input) = parseExp i input startAtt
                let (i,input,_) = matchF i Dollar
                (i,input)
            | _ -> errorR input
        and parseExp i input startAtt =
            match input with
            | Start | LPar | DLPar | Dollar ->
                let (i,input) = parseExp' i input startAtt
                (i,input)
            | _ -> errorR input
        and parseExp' i input startAtt =
            match input with
            | Start | LPar | DLPar ->
                let (i,input,preName) = parsePre i input startAtt
                let (i,input) = parseTrans i input preName
                let (i,input,_) = matchF i Newline
                let (i,input) = parseExp' i input false
                (i,input)
            | Dollar ->
                let (i,input,_) = matchF i Epsilon
                (i,input)
            | _ -> errorR input
        and parsePre i input startAtt =
            match input with
            | Start ->
                let (i,input,_) = matchF i Start
                let (i,input,name) = parsePreState i input true
                (i,input,name)
            | LPar | DLPar ->
                let (i,input,name) = parsePreState i input false
                (i,input,name)
            | _ -> errorR input
        and parsePreState i input startAtt =
            match input with
            | LPar ->
                let (i,input,name) = parseState i input
                add2tree (State (startAtt,name))
                (i,input,name)
            | DLPar ->
                let (i,input,name) = parseFinalState i input
                add2tree (AcceptingState (startAtt,name))
                (i,input,name)
            | _ -> errorR input
        and parseTrans i input targetName =
            match input with
            | Line ->
                let (i,input,transChar) = matchF i Line
                let (i,input,destName) = parseState i input
                add2tree (Transition (targetName,transChar,destName))
                let (i,input) = parseTrans i input targetName
                (i,input)
            | Newline ->
                let (i,input,_) = matchF i Epsilon
                (i,input)
            | _ -> errorR input
        and parseState i input =
            match input with
            | LPar ->
                let (i,input,name) = matchF i LPar
                (i,input,name)
            | _ -> errorR input
        and parseFinalState i input =
            match input with
            | DLPar ->
                let (i,input,name) = matchF i DLPar
                (i,input,name)
            | _ -> errorR input
        if len > 0 then
            let retval = parseS 0 tokens.[0] false
            tree
        else failwith "no input"
    let go str =
        let tokens = lexer str
        let parsed = parser tokens
        parsed
