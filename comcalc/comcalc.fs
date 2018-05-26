namespace Maskiner
module ComCalc =
    open MathParser
    let eval str ftab =
        match parse str ftab with
        | [Result r] -> r
        | _ -> failwith "internal error"
