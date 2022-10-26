module DoMyHomework.Convertor

open OOPList
open MyList

/// Converts an MyList into a OOPList
let rec MyListToOOPList (list: MyList<'Value>) : IList<'Value> =
    match list with
    | Empty -> EmptyList<'Value>() :> IList<'Value>
    | Cons (hd, tl) -> NonEmptyList<'Value>(hd, MyListToOOPList tl)

/// Converts an OOPList into a MyList
let rec OOPListToMyList (list: IList<'Value>) : MyList<'Value> =
    match list with
    | :? EmptyList<'Value> -> Empty
    | :? NonEmptyList<'Value> as list -> Cons(list.Head, OOPListToMyList list.Tail)
    | _ ->
        failwith
            $"Translator.OOPListToMyList: the input data type was expected to be \
                    OOPList+NonEmptyList or OOPList+EmptyList, \
                    but {(list.GetType())} was given"

// Converting Mylist to array
let MyListToArray (list: MyList<'Value>) : 'Value [] =

    let rec subMyListToArray (arr: 'Value []) (list: MyList<'Value>) : 'Value [] =
        match list with
        | Cons (head, tail) -> subMyListToArray(Array.append arr [| head |]) tail
        | Empty -> arr

    subMyListToArray [||] list

// Converting array to Mylist
let ArrayToMyList (arr: array<'Value>) : MyList<'Value> =

    let rec subArrayToMyList (i: int) (arr: 'Value []) : MyList<'Value> =
        if arr.Length <> i then
            Cons(arr[i], (subArrayToMyList(i + 1) arr))
        else
            Empty

    subArrayToMyList 0 arr

/// Converting regular list to MyList
let rec ListToMyList (list: 'A list) : MyList<'A> =
    match list with
    | [] -> Empty
    | head :: tail -> Cons(head, ListToMyList tail)

/// Converting MyList to regular list
let rec MyListToList (mlist: MyList<'A>) : 'A list =
    match mlist with
    | Empty -> []
    | Cons (head, tail) -> head :: MyListToList tail

/// Converting regular list to OOPList
let rec ListToOOPList (list: 'Value list) : IList<'Value> =
    match list with
    | [] -> EmptyList() :> IList<'Value>
    | head :: tail -> NonEmptyList(head, ListToOOPList tail)

/// Converting MyList to regular list
let rec OOPListToList (ooplist: IList<'Value>) : 'Value list =
    match ooplist with
    | :? EmptyList<'Value> -> []
    | :? NonEmptyList<'Value> as list -> list.Head :: OOPListToList list.Tail
    | _ ->
        failwith
            $"Translator.OOPListToList: the input data type was expected to be \
                    OOPList+NonEmptyList or OOPList+EmptyList, \
                    but {(ooplist.GetType())} was given"
