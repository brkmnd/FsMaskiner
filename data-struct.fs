namespace Maskiner
(* Module of higher order functions
 * targeting used data-structures from
 * the .NET library
 * *)
open System.Collections.Generic
type SortedDict<'T0,'T1> =
    static member fold f acc0 (d : SortedDictionary<'T0,'T1>) =
        let mutable elm = d.GetEnumerator()
        let rec exec acc =
            if elm.MoveNext() then
                exec (f acc elm.Current)
            else acc
        exec acc0
    static member foldDual f acc0 (d0 : SortedDictionary<'T0,'T1>) (d1 : SortedDictionary<'T0,'T1>) =
        let mutable elm0 = d0.GetEnumerator()
        let mutable elm1 = d1.GetEnumerator()
        let rec exec acc =
            if elm0.MoveNext() && elm1.MoveNext() then
                exec (f acc elm0.Current elm1.Current)
            else acc
        if d0.Count <> d1.Count then acc0
        else exec acc0
type Dict<'T0,'T1> =
    static member fold f acc0 (d : Dictionary<'T0,'T1>) =
        let mutable elm = d.GetEnumerator()
        let rec exec acc =
            if elm.MoveNext() then
                exec (f acc elm.Current)
            else acc
        exec acc0
type Set<'T> =
    static member fold f acc0 (s : HashSet<'T>) =
        let mutable elm = s.GetEnumerator()
        let rec exec acc =
            if elm.MoveNext() then
                exec (f acc elm.Current)
            else acc
        exec acc0
