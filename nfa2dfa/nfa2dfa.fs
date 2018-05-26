namespace Maskiner
(*
 * Module Nfa2Dfa
 * Module Nfa2Dfa
 * Module Nfa2Dfa
 * Module Nfa2Dfa
 * *)
module Nfa2Dfa =
    open System.Collections.Generic
    type Transition =
        | Epsilon of string
        | Char of string * string
    type State = {
        accepting:bool;
        start:bool;
        transitions:Transition list
        }

    let newState acc s trans =
        {   accepting = acc;
            start = s;
            transitions = trans;
            }
    
    let closure2str (d : SortedDictionary<string,State>) =
        let retval =
            SortedDict<string,State>.fold
                (fun acc x -> acc + "," + x.Key)
                ""
                d
        "{" + retval.[1..] + "}"
    (*
     * echo-functions. For each check if the proper fun
     * is defined in echoes dict. Func is of type string[] -> bool
     * The size of string[] varies  depending what is being echoed.
     * Add : echo s0
     * *)
    let echoSet (echoes : Dictionary<string,System.Func<string [],bool>>) setName (set : SortedDictionary<string,State>) =
        let fName = "set"
        if echoes.ContainsKey(fName) then
            if set.Count = 0 then
                let args = [|setName;"Ø"|]
                let callEcho = echoes.[fName].Invoke(args)
                ()
            else
                let args = [|setName;closure2str set|]
                let callEcho = echoes.[fName].Invoke(args)
                ()
        else ()
    let echoMove (echoes : Dictionary<string,System.Func<string [],bool>>) stateName c (eClosure : SortedDictionary<string,State>) targetName endMarker =
        let fName = "move"
        if not (echoes.ContainsKey("move")) then ()
        elif endMarker then
            let args = [|"_end_";"";"";""|]
            let callEcho = echoes.[fName].Invoke(args)
            ()
        elif eClosure.Count = 0 then
            let args = [|stateName;c;"Ø";"Ø"|]
            let callEcho = echoes.[fName].Invoke(args)
            ()
        else
            let args = [|stateName;c;closure2str eClosure;targetName|]
            let callEcho = echoes.[fName].Invoke(args)
            ()
    let echoDfa (echoes : Dictionary<string,System.Func<string [],bool>>) name (dfa : (string*SortedDictionary<string,State>*Dictionary<string,string>) []) =
        let fName = "dfa"
        let calcStateAtts (states : SortedDictionary<string,State>) =
            let (start,accepting) =
                SortedDict<string,State>.fold
                    (fun (s,a) x ->
                        if s && a then (s,a)
                        elif s then (s,x.Value.accepting)
                        elif a then (x.Value.start,a)
                        else (x.Value.start,x.Value.accepting)
                        )
                    (false,false)
                    states
            if start && accepting then "SA"
            elif start then "S"
            elif accepting then "A"
            else ""
        if not (echoes.ContainsKey(fName)) then ()
        else
            let echo = echoes.[fName]
            let args = [|"dfa";name|]
            let e0 = echo.Invoke(args)
            for (n,s,t) in dfa do
                let args = [|"state";n;calcStateAtts s|]
                let e1 = echo.Invoke(args)
                for t0 in t do
                    let args = [|"transition";t0.Key;t0.Value|]
                    let e2 =
                        echo.Invoke(args)
                    ()
            let eEnd = echo.Invoke([|"_end_"|])
            ()
    (*
     *  Add to state or transition Dictionary.
     *  First check if key exists, if so, do nothing.
     *  These functions are needed since .NET
     *  throws exceptions when inserting a value
     *  with a key that already exists.
     * *)
    let add2transitions (t : Dictionary<string,string>) n0 n =
        if t.ContainsKey(n0) then ()
        else t.Add(n0,n)
    let add2moves (t : SortedDictionary<string,State>) n s =
        if t.ContainsKey(n) then ()
        else t.Add(n,s)
    let add2closure (eClosure : SortedDictionary<string,State>) stateName stateVal =
        if eClosure.ContainsKey(stateName) then eClosure
        else
            eClosure.Add(stateName,stateVal)
            eClosure
    (*
     * Check if 2 sorted dictionaries have equal set of keys.
     * If so, treat them as equal with no regards to the values.
     * Result is bool
     * *)
    let sEQs (states0 : SortedDictionary<string,State>) (states1 : SortedDictionary<string,State>) =
        states0.Count = states1.Count &&
        SortedDict<string,State>.foldDual
            (fun acc x0 x1 -> if acc && x0.Key = x1.Key then true else false)
            true
            states0
            states1
    (* Calculate eClosure on a set of states.
     * Stop if trying to calculate eTransition to a state that does not exist
     * The resulting eClosure is a SortedDictionary<string,State>
     * Sorted since comparison of two closures is then in O(n)
     * *)
    let eClosure states (nfa : Dictionary<string,State>) =
        let rec calc_eClosurePath (newClosure0 : SortedDictionary<string,State>) transitions =
            List.fold
                (fun (newClosure : SortedDictionary<string,State>) transition ->
                    match transition with
                    | Epsilon destName when not (nfa.ContainsKey(destName)) ->
                        failwith
                            ("trying to calc eClosure to state that does "+
                            "not exist")
                    | Epsilon destName when newClosure.ContainsKey (destName)->
                        newClosure
                    | Epsilon destName ->
                        let destState = nfa.[destName]
                        calc_eClosurePath
                            (add2closure newClosure destName destState)
                            destState.transitions
                    | _ -> newClosure
                    )
                newClosure0
                transitions
        SortedDict<string,State>.fold
            (* fold ingoing stateset. Foreach follow any outgoing eTransition to the end
             * Return the newClosure
             * *)
            (fun newClosure state0 ->
                let newClosure = calc_eClosurePath newClosure state0.Value.transitions
                add2closure newClosure state0.Key state0.Value
                )
            (new SortedDictionary<string,State>())
            states
    (* Move on some char c.
     * If two transitions leave the same state and
     * target the same state, only one is recorded.
     * *)
    let move states c (nfa : Dictionary<string,State>)=
        let rec moveState (acc : SortedDictionary<string,State>) = function
            | (Char (c0,n))::transitions when c0 = c ->
                add2moves acc n nfa.[n]
                moveState acc transitions
            | e::transitions ->
                moveState acc transitions
            | _ -> acc
        SortedDict<string,State>.fold
            (fun (acc : SortedDictionary<string,State>) s0 -> moveState acc s0.Value.transitions)
            (new SortedDictionary<string,State>())
            states
    (* Moves on every letter/char in the alphabet
     * Every move is send to echoMove
     * The resulting set is [(string * SortedDictionary<string,State> * Dictionary<string,strint>)]
     * that is an array of stateName * eClosure * transitions
     * *)
    let moveWithAlpha acc0 (curStateName,curState,curStateTransitions : Dictionary<string,string>) nfa echoes alphabet =
        List.fold
            (fun acc c ->
                let moveOnCurWithChar = move curState c nfa
                let eClosureAfterMove = eClosure moveOnCurWithChar nfa
                //Check if calced e-closure is already present as a state
                let (check,newStateName) =
                    let newName0 = sprintf "s%d" (Array.length acc)
                    if moveOnCurWithChar.Count = 0 then
                        (false,newName0)
                    else
                        Array.fold
                            (fun b (n,s,_) ->
                                if sEQs s eClosureAfterMove then
                                    (true,n)
                                else b
                                )
                            (false,newName0)
                            acc
                if moveOnCurWithChar.Count = 0 then
                    echoMove echoes curStateName c eClosureAfterMove "" false
                    add2transitions curStateTransitions c "Ø"
                    acc
                elif check then
                    echoMove
                        echoes
                        curStateName
                        c
                        eClosureAfterMove
                        newStateName
                        false
                    echoSet
                        echoes
                        newStateName
                        eClosureAfterMove
                    add2transitions curStateTransitions c newStateName
                    acc
                else
                    echoMove
                        echoes
                        curStateName
                        c
                        eClosureAfterMove
                        newStateName
                        false
                    echoSet
                        echoes
                        newStateName
                        eClosureAfterMove
                    let t = new Dictionary<string,string>()
                    add2transitions curStateTransitions c newStateName
                    Array.append acc [|(newStateName,eClosureAfterMove,t)|]
                )
            acc0
            alphabet
    (*
     * The actual convertion/entry point of the module
     * The result is an array of dfaStateName * eClosure * transitions.
     * The result is send to echoDfa and then returned
     * *)
    let convert nfaName nfa echoes =
        //Obtain every state with start = true in the nfa
        let startStates =
            Dict.fold
                (fun (acc : SortedDictionary<string,State>) x ->
                    if x.Value.start then acc.Add(x.Key,x.Value); acc
                    else acc
                    )
                (new SortedDictionary<string,State>())
                nfa
        //Calculate the alphabet on the whole incomming nfa
        let alphabet =
            let calc_alpha (acc0 : string list) s0 =
                List.fold
                    (fun (acc : string list) x ->
                        match x with
                        | Char (a,_) when not (List.exists (fun x -> x = a) acc) -> a::acc
                        | _ -> acc
                        )
                    acc0
                    s0
            let retval =
                Dict.fold
                    (fun acc x ->
                        calc_alpha acc x.Value.transitions
                        )
                    []
                    nfa
            //sort is done to make the output more readable
            List.sort retval
        (* Recursively traverses the nfa adding states.
         * The tuple arg is as follows :
         *      name of current state *
         *      NFA-states contained in current state *
         *      transitions from current state to DFA-states
         * *)
        let rec exec states (curStateName,curState,curStateTransitions) i =
            let moves =
                moveWithAlpha
                    states
                    (curStateName,curState,curStateTransitions)
                    nfa
                    echoes
                    alphabet
            let l = Array.length moves
            if l > i then exec moves moves.[i] (i + 1)
            else states
        let t0 =
            let name0 = "s0"
            let eClosure0 = eClosure startStates nfa
            let echoSet = echoSet echoes name0 eClosure0
            (name0,eClosure0,new Dictionary<string,string>())
        let resDfa = exec [|t0|] t0 1
        echoDfa echoes nfaName resDfa
        echoMove echoes "" "" (new SortedDictionary<string,State>()) "" true
        resDfa
