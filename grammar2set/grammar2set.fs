namespace Maskiner
module Grammar2Set =
    open System.Collections.Generic
    open GrammarParser
    type Nullable =
        | T
        | F
        | NullableOf of string list
    type First =
        | FirstOf of string
        | TermSymbol of string
    type FollowConst =
        | FollowOf of string
        | FirstSet of HashSet<ProdExp>
        | SolvedSet of ProdExp list
    (* Misc functions for adding to sets
     * *)
    let add2solved (solved : Dictionary<string,HashSet<ProdExp>>) n vList =
        if solved.ContainsKey(n) then
            //Iterate through old val and new val only adding values
            //That are not present in old val. Return bool * solved
            //where bool checks if new elements are added.
            let (newAdded,_) =
                List.fold
                    (fun (newAdded,set : HashSet<ProdExp>) elm ->
                        (set.Add(elm) || newAdded,set)
                        )
                    (false,solved.[n])
                    vList
            (newAdded,solved)
        else
            let newSet =
                List.fold
                    (fun (set : HashSet<ProdExp>) elm ->
                        let add2set = set.Add(elm)
                        set
                        )
                    (new HashSet<ProdExp>())
                    vList
            solved.Add(n,newSet)
            (true,solved)
    let addSet2solved (solved : Dictionary<string,HashSet<ProdExp>>) n set =
        if solved.ContainsKey(n) then
            let added =
                Set<ProdExp>.fold
                    (fun added elm -> solved.[n].Add(elm) || added)
                    false
                    set
            (added,solved)
        else
            solved.Add(n,set)
            (true,solved)
    (* Calc Nullable bools
     * The algorithm used differs from fixpoint alg.
     * normally used
     * Return : Dictionary of prod * bool
     * *)
    let calcNullable (grammar : Dictionary<string,(ProdExp list) list>) =
        let add2solved (solvedNullables : Dictionary<string,bool>) n v =
            if solvedNullables.ContainsKey(n) then solvedNullables
            else solvedNullables.Add(n,v); solvedNullables
        let add2unsolved (unsolvedNullables : Dictionary<string,Nullable list>) n v =
            if unsolvedNullables.ContainsKey(n) then unsolvedNullables
            else unsolvedNullables.Add(n,v); unsolvedNullables
        // Iterate through a set of single right-side.
        // - If terminal found, then nullable = false(short-circuit)
        // - If nonTerm found, add that to a NullableOf list
        // this list is nullable if every nonTerm is
        // nullable - thus conjunctions
        // - If nonTerm, and the grammar does not contains this as key
        // then fail
        let rec initIter_rExps acc = function
            | (Term _)::rExp -> F
            | (NonTerm nt)::rExp when not (grammar.ContainsKey(nt)) ->
                let msg = sprintf "nullable: the production '%s' doesn not exist" nt
                failwith msg
            | (NonTerm nt)::rExp ->
                initIter_rExps (nt::acc) rExp
            | [] -> NullableOf acc
            | _ -> failwith "nullable: garbage in grammar"
        // Iterate through all the right-sides of a production
        // If empty right-side found -> nullable = true
        // If set of expressions found, then iterate them
        // using rExps iter above.
        let initIter_rSide (rSide : (ProdExp list) list) =
            List.fold
                (fun nullList rightSide ->
                    match (rightSide,nullList) with
                    | (_,T::nList) -> T::nList
                    | ([],_) -> T::nullList
                    | (rExps,_) ->
                        (initIter_rExps [] rExps)::nullList
                    )
                []
                rSide
        // Init-iteration of grammar
        // type is Dictionary of string * Nullable list
        let initIter =
            Dict<string,(ProdExp list) list>.fold
                (fun (nullableS : Dictionary<string,Nullable list>) prod ->
                    let nullable = initIter_rSide prod.Value
                    nullableS.Add(prod.Key,nullable)
                    nullableS
                    )
                (new Dictionary<string,Nullable list>())
                grammar
        // Solve every part of single dependency
        // Short circuit when rExp = false
        let rec solveDep_rExps (solved : Dictionary<string,bool>) (retlist,isNullable) = function
            | pName::rExps when solved.ContainsKey(pName) ->
                if solved.[pName] then
                    solveDep_rExps solved (retlist,true) rExps
                else ([],false)
            | pName::rExps ->
                solveDep_rExps solved (pName::retlist,isNullable) rExps
            | [] -> (retlist,isNullable)
        // Solve right-sides of production
        // Each set of deps is solved in disjunctions.
        // Thus if one right-side = true, short circuit
        let rec solveDep_rSides (solved : Dictionary<string,bool>) (retlist,isNullable) = function
            | T::rSides -> ([],true)
            | F::rSides ->
                solveDep_rSides solved (retlist,false) rSides
            | (NullableOf deps)::rSides ->
                let (newDeps,isNullable0) =
                    solveDep_rExps solved ([],true) deps
                let isSolved = List.length newDeps = 0
                if isSolved && isNullable0 then
                    ([],true)
                elif isSolved then
                    solveDep_rSides solved (retlist,false) rSides
                else
                    solveDep_rSides
                        solved
                        ((NullableOf newDeps)::retlist,false)
                        rSides
            | [] -> (retlist,isNullable)
        // Solve what is left of dependencies from initIter
        // iterate on a solved and an unsolved dict
        // foreach iteration try to solve unsolved
        // using values from solved. If no unsolved
        // is solvable, then fail since circular grammar
        // is solved on. If unsolved empty, then end
        let rec solveDependencies (solved : Dictionary<string,bool>) (unsolved : Dictionary<string,Nullable list>) =
            let (newSolved,newUnsolved) =
                Dict<string,Nullable list>.fold
                    (fun (newSolved,newUnsolved) prod ->
                        let (retlist,isNullable) =
                            solveDep_rSides
                                solved
                                ([],false)
                                prod.Value
                        let isSolved = List.length retlist = 0
                        if isSolved then
                            (add2solved newSolved prod.Key isNullable,newUnsolved)
                        else
                            (newSolved,add2unsolved newUnsolved prod.Key retlist)
                        )
                    (solved,new Dictionary<string,Nullable list>())
                    unsolved
            if unsolved.Count = newUnsolved.Count then
                failwith "nullable: circular error"
            elif newUnsolved.Count > 0 then
                solveDependencies newSolved newUnsolved
            else
                newSolved
        solveDependencies (new Dictionary<string,bool>()) initIter
    (* Calc FirstSets
     * The alg. used is a fixpoint one described above functions below.
     * Furthermore it is described in Introduction to Compiler Design
     * by Torben Mogensen
     * Return : Dictionary of strin * HashSet of terminals
     * *)
    let calcFirst grammar (nullableS : Dictionary<string,bool>) =
        let isNullable pName =
            if nullableS.ContainsKey(pName) then nullableS.[pName]
            else failwith (sprintf "first: Nullable(%s) not found" pName)
        let add2set (sets : Dictionary<string,First>) pName v =
            if sets.ContainsKey(pName) then ()
            else sets.Add(pName,v)
        let add2unsolved (sets : Dictionary<string,First list>) pName v =
            if sets.ContainsKey(pName) then sets
            else sets.Add(pName,v); sets
        // Iterate through grammar adding as follows
        // - First(t) = t where t is a terminal
        // - First(a b) = First(a) U First(b) if Nullable(a)
        // - First(a b) = First(a) if not Nullable(a)
        // The return is a list of FirstOf nonTerm, TermSymbol terminal
        let rec initIter_exps initSet = function
            | (NonTerm pName)::rExp when pName|>isNullable ->
                initIter_exps ((FirstOf pName)::initSet) rExp
            | (NonTerm pName)::rExp ->
                (FirstOf pName)::initSet
            | (Term t)::rExp ->
                (TermSymbol t)::initSet
            | Dollar::rExp ->
                initIter_exps initSet rExp
            | [] -> initSet
        let initIter_rSide =
            List.fold
                initIter_exps
                []
        let initIter =
            Dict<string,(ProdExp list) list>.fold
                (fun unsolved prod ->
                    add2unsolved unsolved prod.Key (initIter_rSide prod.Value)
                    )
                (new Dictionary<string,First list>())
                grammar
        // Start with empty solved. First add from unsolved every TermSymbol.
        // Then iterate adding from other first sets until no new elms are added
        // - fixpoint is reached and solved are returned
        // The return is a Dictionary of HashSets containing first symbols (terminals)
        let solveDependencies (unsolved : Dictionary<string,First list>) =
            let (solved,newUnsolved) =
                Dict<string,First list>.fold
                    (fun (solved,newUnsolved) prod ->
                        let (solvedSet,unsolvedSet) =
                            List.fold
                                (fun (sSet : HashSet<ProdExp>,uSet) elm ->
                                    match elm with
                                    | TermSymbol t ->
                                        let add = sSet.Add(Term t)
                                        (sSet,uSet)
                                    | _ -> (sSet,elm::uSet)
                                    ) 
                                (new HashSet<ProdExp>(),[])
                                prod.Value
                        let (_,solved) = addSet2solved solved prod.Key solvedSet
                        (solved,add2unsolved newUnsolved prod.Key unsolvedSet)
                        )
                    (new Dictionary<string,HashSet<ProdExp>>(),new Dictionary<string,First list>())
                    unsolved
            let rec fixpointIter (solved : Dictionary<string,HashSet<ProdExp>>) =
                let newAdded =
                    Dict<string,First list>.fold
                        (fun newAdded prod ->
                            List.fold
                                (fun newAdded unsolved ->
                                    match unsolved with
                                    | FirstOf pName when solved.ContainsKey(pName) ->
                                        let (nAdded,_) =
                                            addSet2solved
                                                solved
                                                prod.Key
                                                solved.[pName]
                                        nAdded
                                    | _ -> newAdded
                                    )
                                newAdded
                                prod.Value
                            )
                        false
                        newUnsolved
                if newAdded then fixpointIter solved
                else solved
            fixpointIter solved
        solveDependencies initIter
    (* Calc Follow
     * The alg. used is a fixpoint one described above some of the functions
     * below. Or it can be found in Introduction to Compiler Design by
     * Torben Mogensen
     * Return : Dictionary of string * HashSet of terminals
     * *)
    let calcFollow grammar (firstS : Dictionary<string,HashSet<ProdExp>>) (nullableS : Dictionary<string,bool>) =
        //Misc functions
        let isNullable pName =
            if nullableS.ContainsKey(pName) then nullableS.[pName]
            else
                let msg =
                    sprintf
                        "follow: nullableS does not contain '%s'"
                        pName 
                failwith msg
        let isRestNullable rList =
            List.fold
                (fun check elm ->
                    match elm with
                    | Term _ | Dollar -> false
                    | NonTerm pName -> check && isNullable pName 
                    )
                true
                rList
        let getFirst pName =
            if firstS.ContainsKey(pName) then firstS.[pName]
            else
                let msg =
                    sprintf
                        "follow: firstS does not contain '%s'"
                        pName
                failwith msg
        let rec getFirstOfRest (fSet : HashSet<ProdExp>) = function
            | (NonTerm nt)::rest when nt|>isNullable ->
                let add2 = fSet.UnionWith(getFirst nt)
                getFirstOfRest fSet rest
            | (NonTerm nt)::rest ->
                let add2 = fSet.UnionWith(getFirst nt)
                fSet
            | (Term t)::rest ->
                let add2 = fSet.Add(Term t)
                fSet
            | Dollar::rest ->
                let add2 = fSet.Add(Dollar)
                fSet
            | [] -> fSet
        let createFirstSetFromSingle v =
            let retval = new HashSet<ProdExp>()
            let b = retval.Add(v)
            FirstSet retval
        let add2consts (consts : Dictionary<string,FollowConst list>) n v =
            if consts.ContainsKey(n) then
                let oldV = consts.[n]
                consts.[n] <- v::oldV
                consts
            else
                consts.Add(n,[v])
                consts
        let add2unsolved (unsolved : Dictionary<string,FollowConst list>) n v =
            if unsolved.ContainsKey(n) then unsolved
            else unsolved.Add(n,v); unsolved
        // Add startproduction S -> E $ to grammar
        // For now this rely on that Add adds in order
        // for enumerator to a dictionary.
        // This behavior is not promised, but I
        // haven't seen otherwise
        let (newGrammar,_) =
            let d = new Dictionary<string,(ProdExp list) list>()
            Dict<string,(ProdExp list) list>.fold
                (fun
                    (acc : Dictionary<string,(ProdExp list) list>,fElm)
                    (prod : KeyValuePair<string,(ProdExp list) list>) ->
                        let addFirst =
                            if fElm then
                                acc.Add("_",[[NonTerm prod.Key;Dollar]])
                            else ()
                        acc.Add(prod.Key,prod.Value)
                        (acc,false)
                        )
                (d,true)
                grammar
        // Add constraints from right-side exps.
        // If nonTerm1 nonTerm2 and rest is nullable, then
        // add both the production itself as Follow and
        // add First(nonTerm2) to Follow(nonTerm1)
        // If nonTerm1 nonTerm2 then add First(nonTerm2) to
        // Follow(nonTerm1)
        // If nonTerm term then add First(term) = term to
        // Follow(nonTerm). $ is a special term
        // If nonTerm a and Nullable(a), then add the prod
        // itself to Follow(nonTerm)
        let rec constraints_rExp pName consts = function
            | (NonTerm nt)::(NonTerm ntF)::rExp when isRestNullable ((NonTerm ntF)::rExp) ->
                let rest = (NonTerm ntF)::rExp
                let fSetI = getFirstOfRest (new HashSet<ProdExp>()) rest
                let fSet = FirstSet fSetI
                let consts = add2consts consts nt fSet
                let consts = add2consts consts nt (FollowOf pName)
                constraints_rExp pName consts rest
            | (NonTerm nt)::(NonTerm ntF)::rExp ->
                let rest = (NonTerm ntF)::rExp
                let fSetI = getFirstOfRest (new HashSet<ProdExp>()) rest
                let fSet = FirstSet fSetI
                constraints_rExp
                    pName
                    (add2consts consts nt fSet)
                    rest
            | (NonTerm nt)::(Term t)::rExp ->
                let fSet = createFirstSetFromSingle (Term t)
                constraints_rExp pName (add2consts consts nt fSet) rExp
            | (NonTerm nt)::ProdExp.Dollar::rExp ->
                let fSet = createFirstSetFromSingle (ProdExp.Dollar)
                constraints_rExp pName (add2consts consts nt fSet) rExp
            | (NonTerm nt)::rExp when isRestNullable rExp ->
                add2consts consts nt (FollowOf pName)
            | [] -> consts
            | t::rExp -> constraints_rExp pName consts rExp
        let constraints_rSide (constraints) (prod : KeyValuePair<string,(ProdExp list) list>) =
            List.fold
                (constraints_rExp prod.Key)
                constraints
                prod.Value
        let constraints =
            Dict<string,(ProdExp list) list>.fold
                constraints_rSide
                (new Dictionary<string,FollowConst list>())
                newGrammar
        let solveDependencies (consts : Dictionary<string,FollowConst list>) =
            // Add first from constraints to solved.
            // When adding use a bool to check if
            // new elements are added
            let (solved,newConsts) =
                Dict<string,FollowConst list>.fold
                    (fun (solvedFirst,newConsts) prod ->
                        let newConstSet =
                            List.fold
                                (fun newConsts cons ->
                                    match cons with
                                    | FirstSet set ->
                                        let add2new =
                                            addSet2solved
                                                solvedFirst
                                                prod.Key
                                                set
                                        newConsts
                                    | _ -> cons::newConsts
                                    )
                                []
                                prod.Value
                        if List.length newConstSet > 0 then
                            (   solvedFirst,
                                add2unsolved
                                    newConsts
                                    prod.Key
                                    newConstSet
                                )
                        else
                            (solvedFirst,newConsts)
                        )
                    (new Dictionary<string,HashSet<ProdExp>>(),
                     new Dictionary<string,FollowConst list>()
                     )
                    consts
            // Then iterate adding followSets until
            // no new elements are added. Iterate over
            // the whole constraints-set
            let rec fixpointIter (solved : Dictionary<string,HashSet<ProdExp>>) =
                let newAdded =
                    Dict<string,FollowConst list>.fold
                        (fun newAdded prod ->
                            List.fold
                                (fun newAdded cons ->
                                    match cons with
                                    | FollowOf pName when solved.ContainsKey(pName) ->
                                        let (nAdded,_) =
                                            addSet2solved
                                                solved
                                                prod.Key
                                                solved.[pName]
                                        nAdded
                                    | _ -> newAdded
                                    )
                                newAdded
                                prod.Value
                            )
                        false
                        newConsts
                if newAdded then fixpointIter solved
                else solved
            fixpointIter solved
        solveDependencies constraints
