open Maskiner
open GrammarParserExt
open System.Collections.Generic
let fopen name =
    let l = System.IO.File.ReadAllLines ("tests_parser/" + name)
    Array.fold
        (fun acc line -> acc + line + "\n")
        ""
        l
let fwrite str =
    let name = "fsparser.out"
    System.IO.File.WriteAllText(name,str)

let createData str =
    match parse str with
    | [DataProds prods;DataTokens tokens;DataBTokens btokens;DataPrecs precs;DataAssocs assocs;DataGroups groups] ->
        (prods,tokens,btokens,precs,assocs,groups)
    | [EmptyTree] -> failwith "empty tree"
    | (DataProds _)::(DataTokens _)::(DataPrecs _)::(DataAssocs _)::tree ->
        failwith (sprintf "some error : %A" tree)
    | tree ->
        printfn "treeTop : %A" tree.[0]
        failwith "some other error"

let testParser str =
    let (prods,tokens,btokens,precs,assocs,groups) = createData str
    for prod in prods do
        printfn "prod %s -> %A" prod.Key prod.Value
    for prec in precs do
        printfn "prec %s %d" prec.Key prec.Value
    for assoc in assocs do
        printfn "assoc %s %s" assoc.Key assoc.Value
    for token in tokens do
        printfn "token : %s as %s" token.Key token.Value
    for btoken in btokens do
        printfn "!token : %s" btoken
    for group in groups do
        printfn "group '%s' : %A" group.Key group.Value

testParser (fopen "test_comcalc")
