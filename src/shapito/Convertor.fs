module DoMyHomework.Convertor

open OOPList
open MyList

/// Converts an MyList into a OOPList
let rec MyListToOOPList (list: MyList<'value>) : IList<'value> =
    match list with
    | Empty -> EmptyList<'value>() :> IList<'value>
    | Cons (hd, tl) -> NonEmptyList<'value>(hd, MyListToOOPList tl)

/// Converts an OOPList into a MyList
let rec OOPListToMyList (list: IList<'value>) : MyList<'value> =
    match list with
    | :? EmptyList<'value> -> Empty
    | :? NonEmptyList<'value> as list -> Cons(list.Head, OOPListToMyList list.Tail)
    | _ -> failwith $"Translator.OOPListToMyList: the input data type was expected to be \
                    OOPList+NonEmptyList or OOPList+EmptyList, \
                    but {(list.GetType())} was given"

// Converting Mylist to array
let MyListToArr (list: MyList<'value>) : 'value [] =

    let rec SubMyListToArr (arr: 'value []) (list: MyList<'value>) : 'value [] =
        match list with
        | Cons (head, tail) -> SubMyListToArr(Array.append arr [| head |]) tail
        | Empty -> arr

    SubMyListToArr [||] list

// Converting array to Mylist
let arrToMyList (arr: array<'value>) : MyList<'value> =

    let rec SubArrToMyList (i: int) (arr: 'value []) : MyList<'value> =
        if arr.Length <> i then
            Cons(arr[i], (SubArrToMyList(i + 1) arr))
        else
            Empty

    SubArrToMyList 0 arr

/// Converting regular list to MyList
let rec ListToMyList (list: 'a list) : MyList<'a> =
    match list with
    | [] -> Empty
    | head :: tail -> Cons(head, ListToMyList tail)

/// Converting MyList to regular list
let rec MyListToList (mlist: MyList<'a>) : 'a list =
    match mlist with
    | Empty -> []
    | Cons (head, tail) -> head :: MyListToList tail

/// Converting regular list to OOPList
let rec ListToOOPList (list: 'value list) : IList<'value> =
    match list with
    | [] -> EmptyList() :> IList<'value>
    | head :: tail -> NonEmptyList(head, ListToOOPList tail)

/// Converting MyList to regular list
let rec OOPListToList (ooplist: IList<'value>) : 'value list =
    match ooplist with
    | :? EmptyList<'value> -> []
    | :? NonEmptyList<'value> as list -> list.Head :: OOPListToList list.Tail
    | _ -> failwith $"Translator.OOPListToList: the input data type was expected to be \
                    OOPList+NonEmptyList or OOPList+EmptyList, \
                    but {(ooplist.GetType())} was given"
