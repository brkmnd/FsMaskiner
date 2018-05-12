open Maskiner
open System.Collections.Generic
let nfa2dfa (name,nfa,echo) =
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

let printMove (args : string[]) =
    let print =
        if args.[0] <> "_end_" then
            printfn "move(%s,%s) = %s = %s" args.[0] args.[1] args.[2] args.[3]
        else printfn ""
    true
let printDfa (args : string[]) =
    let print =
        if args.[0] = "dfa" then
                printfn "dfa from '%s'" args.[1]
        elif args.[0] = "state" then
            if args.[2] = "SA" then
                printfn "start ((%s))" args.[1]
            elif args.[2] = "S" then
                printfn "start (%s)" args.[1]
            elif args.[2] = "A" then
                printfn "((%s))" args.[1]
            else
                printfn "(%s)" args.[1]
        elif args.[0] = "transition" then
            if args.[2] = "Ø" then
                printfn "\t-%s> Ø" args.[1]
            else
                printfn "\t-%s> (%s)" args.[1] args.[2]
        elif args.[0] = "_end_" then printfn "\n"
    true
let echo = new Dictionary<string,System.Func<string[],bool>>()

//echo.Add("move",new System.Func<string[],bool>(printMove))
echo.Add("dfa",new System.Func<string[],bool>(printDfa))

let eClosureTest1 =
    "start (1) -eps>(2) -eps>(3)\n"+
    "(2) -eps>(4) -eps>(5)\n"+
    "(3) -eps>(5) -eps>(4)\n"+
    "(4) -a>(6) -b>(6)\n"+
    "(5) -a>(6) -b>(6)\n"+
    "((6))\n"
let status_eClosureTest1 = nfa2dfa("eClosureTest",eClosureTest1,echo)
printfn "status(test1) : %s" status_eClosureTest1


let fejl_nfa1 =
    "start(1) -eps>(2)\n"+
    "(2) -eps>(1)\n"
let status_fejl1 = nfa2dfa("fejl1",fejl_nfa1,echo)
printfn "status(fejl1) : %s" status_fejl1

let fejl_nfa2 =
    "start(1) -eps>(2) -eps>(3)\n"+
    "(2) -b>(6) -a>(6)\n"+
    "(3) -a>(2) -eps>(4)\n"+
    "(4) -a>(4) -b>(4) -eps>(6)\n"+
    "((6)) -eps>(4)\n"
let status_fejl2 = nfa2dfa("fejl2",fejl_nfa2,echo)
printfn "status(fejl2) : %s" status_fejl2

let fejl_nfa3 =
    "start(1) -eps>(1)\n"
let status_fejl3 = nfa2dfa("fejl3",fejl_nfa3,echo)
printfn "status(fejl3) : %s" status_fejl3

let nfa17 =
    "start(1) -eps>(2) -eps>(4)\n"+
    "(2) -a>(3)\n"+
    "(3) -b>(2) -eps>(6)\n"+
    "(4) -a>(5)\n"+
    "(5) -eps>(2) -b>(6)\n"+
    "((6))\n"
printfn "\nnfa14 to dfa17"
let status17 = nfa2dfa("nfa17",nfa17,echo)

let nfa16 =
    "start(1) -eps>(2) -eps>(4)\n"+
    "(2) -b>(3)\n"+
    "(3) -a>(3) -eps>(6)\n"+
    "(4) -eps>(5) -b>(5)\n"+
    "(5) -eps>(3) -b>(6) -a>(4)\n"+
    "((6))\n"
printfn "nfa16 to dfa16"
let status16 = nfa2dfa("nfa16",nfa16,echo)

let nfa15 =
    "start (1) -eps>(2) -eps> (4)\n"+
    "(2) -a> (3)\n"+
    "(3) -b>(3) -eps>(6)\n"+
    "(4) -a>(4) -b>(5)\n"+
    "(5) -eps>(4) -eps>(6)\n"+
    "((6))\n"
printfn "nfa15 to dfa15"
let status15 = nfa2dfa("nfa15",nfa15,echo)

let nfa_test1 =
    "start (1) -a>(2) -a>(3)\n"+
    "(2) -b>(1) -eps>(3)\n"+
    "((3))\n"
