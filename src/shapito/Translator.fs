module shapito.Translator

open OOPList
open MyList

/// Converts an MyList into a OOPList
let rec MyListToMyOOPList (lst: MyList<'value>): IList<'value> =
    match lst with
    | Empty -> EmptyList<'value>() :> IList<'value>
    | Cons (hd, tl) -> NonEmptyList<'value>(hd, MyListToMyOOPList tl)

/// Converts an OOPList into a MyList
let rec OOPListToMyList (list: IList<'value>): MyList<'value> =
    match list with
    | :? EmptyList<'value> -> Empty
    | :? NonEmptyList<'value> as lst -> Cons(lst.Head, OOPListToMyList lst.Tail)
    | _ -> failwith "fail in OOPList concatenation"

// Converting Mylist to array
let MyListToArr (list: MyList<'value>) : 'value[] =

    let rec SubMyListToArr (arr: 'value[]) (list: MyList<'value>): 'value[] =
        match list with
        | Cons (head, tail) -> SubMyListToArr(Array.append arr [| head |]) tail
        | Empty -> arr

    SubMyListToArr [||] list

// Converting array to Mylist
let arrToMyList (arr: 'value[]) : MyList<'value> =

    let rec SubArrToMyList (i: int) (arr: 'value[]): MyList<int> =
        if arr.Length <> i then
            // This dot is not redundant!
            Cons(arr[i], (SubArrToMyList (i + 1) arr))
        else
            Empty

    SubArrToMyList 0 arr

/// Converting regular list to MyList
let rec listToMyList (lst: 'a list): MyList<'a> =
    match lst with
    | [] -> Empty
    | head :: tail -> Cons(head, listToMyList tail)

/// Convertung MyList to regular list
let rec MyListToList (lst: MyList<'a>): 'a list =
    match lst with
    | Empty -> []
    | Cons (head, tail) -> head :: MyListToList tail
