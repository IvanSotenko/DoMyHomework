module shapito.Translator

open OOPList
open LinkedList

let rec fromMyListToMyOOPList lst =
    match lst with
    | Empty -> EmptyList<'value>() :> IList<'value>
    | Cons (hd, tl) -> NonEmptyList<'value>(hd, fromMyListToMyOOPList tl)

let rec fromOOPListToMyList (list: IList<'value>) =
    match list with
    | :? EmptyList<'value> -> Empty
    | :? NonEmptyList<'value> as lst ->
        Cons ( lst.Head, fromOOPListToMyList lst.Tail)
    | _ -> failwith "fail in OOPList concatenation"
