module DoMyHomework.Convertor

open MyList

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
