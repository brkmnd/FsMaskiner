open Maskiner.PropParser
let fopen name =
    let l = System.IO.File.ReadAllLines ("parser_tests/" + name)
    Array.fold
        (fun acc line -> acc + line + "\n")
        ""
        l
let printTest fName =
    printfn "test (%s)" fName
    let input = fopen fName
    let (vtab,tree) =
        try parse input with
         | Failure msg ->
             let stdColor = System.Console.ForegroundColor
             System.Console.ForegroundColor <- System.ConsoleColor.Red
             printfn "%s" msg
             System.Console.ForegroundColor <- stdColor 
             ([],EmptyTree)
    printfn "input: %s" input
    printfn "vtab: %A" vtab
    printfn "tree: %A" tree
    printfn ""

printTest "test1"
printTest "test2"
printTest "test3"
printTest "test4"
printTest "test5"
printTest "alt_syntax1"
printTest "testFail1"
printTest "testFail2"
printTest "testFail3"
printTest "testFail4"
printTest "testFail5"
