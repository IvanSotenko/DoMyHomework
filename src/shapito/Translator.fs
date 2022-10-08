module shapito.Translator

open OOPList
open LinkedList

/// Converts an MyList into a OOPList
let rec fromMyListToMyOOPList lst =
    match lst with
    | Empty -> EmptyList<'value>() :> IList<'value>
    | Cons (hd, tl) -> NonEmptyList<'value>(hd, fromMyListToMyOOPList tl)

/// Converts an OOPList into a MyList
let rec fromOOPListToMyList (list: IList<'value>) =
    match list with
    | :? EmptyList<'value> -> Empty
    | :? NonEmptyList<'value> as lst -> Cons(lst.Head, fromOOPListToMyList lst.Tail)
    | _ -> failwith "fail in OOPList concatenation"
