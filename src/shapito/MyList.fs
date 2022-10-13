module MyList

type MyList<'value> =
    | Cons of head: 'value * tail: MyList<'value>
    | Empty

let rec concat (list1: MyList<'value>) (list2: MyList<'value>) : MyList<'value> =
    match list1 with
    | Cons (head, tail) -> Cons(head, concat tail list2)
    | Empty -> list2
