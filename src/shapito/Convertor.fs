module DoMyHomework.Convertor

open OOPList
open MyList

/// Converts an MyList into a OOPList
let rec MyListToOOPList (lst: MyList<'value>) : IList<'value> =
    match lst with
    | Empty -> EmptyList<'value>() :> IList<'value>
    | Cons (hd, tl) -> NonEmptyList<'value>(hd, MyListToOOPList tl)

/// Converts an OOPList into a MyList
let rec OOPListToMyList (list: IList<'value>) : MyList<'value> =
    match list with
    | :? EmptyList<'value> -> Empty
    | :? NonEmptyList<'value> as lst -> Cons(lst.Head, OOPListToMyList lst.Tail)
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
let rec listToMyList (lst: 'a list) : MyList<'a> =
    match lst with
    | [] -> Empty
    | head :: tail -> Cons(head, listToMyList tail)

/// Converting MyList to regular list
let rec MyListToList (mlst: MyList<'a>) : 'a list =
    match mlst with
    | Empty -> []
    | Cons (head, tail) -> head :: MyListToList tail

/// Converting regular list to OOPList
let rec listToOOPList (lst: 'value list) : IList<'value> =
    match lst with
    | [] -> EmptyList() :> IList<'value>
    | head :: tail -> NonEmptyList(head, listToOOPList tail)

/// Converting MyList to regular list
let rec OOPListToList (ooplst: IList<'value>) : 'value list =
    match ooplst with
    | :? EmptyList<'value> -> []
    | :? NonEmptyList<'value> as lst -> lst.Head :: OOPListToList lst.Tail
    | _ -> failwith $"Translator.OOPListToList: the input data type was expected to be \
                    OOPList+NonEmptyList or OOPList+EmptyList, \
                    but {(ooplst.GetType())} was given"
