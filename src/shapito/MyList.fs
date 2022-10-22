module DoMyHomework.MyList

type MyList<'value> =
    | Cons of head: 'value * tail: MyList<'value>
    | Empty

let rec ofList (list: 'a list) : MyList<'a> =
    match list with
    | [] -> Empty
    | head :: tail -> Cons(head, ofList tail)

let rec toList (mlist: MyList<'a>) : 'a list =
    match mlist with
    | Empty -> []
    | Cons (head, tail) -> head :: toList tail


let rec concat (list1: MyList<'value>) (list2: MyList<'value>) : MyList<'value> =
    match list1 with
    | Cons (head, tail) -> Cons(head, concat tail list2)
    | Empty -> list2


let toSet (list: MyList<'a>): Set<'a> =
    let rec SetOfMyListSub list st =
        match list with
        | Empty -> Set.empty
        | Cons (head, tail) -> st + Set.empty.Add(head) + SetOfMyListSub tail st

    SetOfMyListSub list Set.empty


let length (list: MyList<'a>): int =
    let rec lengthSub list n =
        match list with
        | Empty -> n
        | Cons (_, tail) -> lengthSub tail (n + 1)
    lengthSub list 0
