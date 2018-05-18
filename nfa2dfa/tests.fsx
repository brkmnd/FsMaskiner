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

let fopen name =
    let (found,l) =
        try (true,System.IO.File.ReadAllLines ("tests/" + name)) with
        | _ -> (false,[||])
    let retstr =
        Array.fold
            (fun acc line -> acc + line + "\n")
            ""
            l
    (found,retstr)
let printSet (args : string[]) =
    printfn "%s = %s" args.[0] args.[1]
    true
let printMove (args : string[]) =
    let print =
        if args.[0] <> "_end_" then
            printfn "move(%s,%s) = %s = %s" args.[0] args.[1] args.[2] args.[3]
        else printfn ""
    true

let mutable lastDfaName = ""
let forRes = new Dictionary<string,string>()
let insert2forRes line =
    if forRes.ContainsKey(lastDfaName) then
        let v = forRes.[lastDfaName]
        forRes.[lastDfaName] <- v + line + "\n"

let printDfa (args : string[]) =
    let print =
        if args.[0] = "dfa" then
                let dfaName = args.[1]
                lastDfaName <- dfaName
                if not (forRes.ContainsKey(dfaName)) then
                    forRes.Add(dfaName,"")
                printfn "dfa from '%s'" args.[1]
        elif args.[0] = "state" then
            if args.[2] = "SA" then
                let out = sprintf "start ((%s))" args.[1]
                insert2forRes(out)
                printfn "%s" out
            elif args.[2] = "S" then
                let out = sprintf "start (%s)" args.[1]
                insert2forRes(out)
                printfn "start (%s)" args.[1]
            elif args.[2] = "A" then
                let out = sprintf "((%s))" args.[1]
                insert2forRes(out)
                printfn "%s" out
            else
                let out = sprintf "(%s)" args.[1]
                insert2forRes(out)
                printfn "%s" out
        elif args.[0] = "transition" then
            if args.[2] = "Ø" then
                let out = sprintf "    -%s>Ø" args.[1]
                insert2forRes(out)
                printfn "%s" out
            else
                let out = sprintf "    -%s>(%s)" args.[1] args.[2]
                insert2forRes(out)
                printfn "%s" out
        elif args.[0] = "_end_" then
            let out = "\n"
            printfn "%s" out
    true
let echo = new Dictionary<string,System.Func<string[],bool>>()

//echo.Add("move",new System.Func<string[],bool>(printMove))
echo.Add("set",new System.Func<string [],bool>(printSet))
echo.Add("dfa",new System.Func<string[],bool>(printDfa))

let hr = List.fold (fun acc a -> acc + a) "" (List.init 60 (fun i -> "-"))

let printTest fName =
    printfn "test (%s)" fName
    let (fileFound,inputStr) = fopen fName
    let (resFound,resStr) = fopen (fName + ".res")
    if fileFound then
        let status = nfa2dfa(fName,inputStr,echo)
        let status =
            if status <> "" then status
            elif resFound then
                if forRes.[fName] = resStr then
                    "compare to res: success"
                else
                    sprintf "compare to res: failure compared to \n%s" resStr
            else "success"
        printfn "status(%s) = %s" fName status
        printfn "%s" hr
    else
        printfn "file '%s' not found" fName

printTest "test1"
printTest "test2"
printTest "test3"
printTest "test10"
printTest "test11"
printTest "test_comm1"
printTest "test_loop1"
printTest "test_internet1"
printTest "test_internet2"
printTest "test_internet3"
printTest "test_ex15"
printTest "test_ex16"
printTest "test_ex17"
printTest "test_ex18"
