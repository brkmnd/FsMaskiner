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
            try ("",Nfa2Dfa.convert true name parsed echo) with
            | Failure msg -> (msg,[||])
    msg1

let fopen name =
    let l = System.IO.File.ReadAllLines ("tests/" + name)
    Array.fold
        (fun acc line -> acc + line + "\n")
        ""
        l
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

let hr = List.fold (fun acc a -> acc + a) "" (List.init 60 (fun i -> "-"))

let printTest fName =
    printfn "test (%s)" fName
    let str = fopen fName
    let status = nfa2dfa(fName,str,echo)
    printfn "status(%s) = %s" fName status
    printfn "%s" hr

printTest "test1"
printTest "test2"
printTest "test3"
printTest "test_ex15"
printTest "test_ex16"
printTest "test_ex17"
printTest "test10"
printTest "test11"
printTest "test_comm1"
