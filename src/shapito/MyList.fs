module DoMyHomework.MyList

type MyList<'value> =
    | Cons of head: 'value * tail: MyList<'value>
    | Empty


/// That function insert second list to the tail of first
let rec concat (list1: MyList<'value>) (list2: MyList<'value>) : MyList<'value> =
    match list1 with
    | Cons (head, tail) -> Cons(head, concat tail list2)
    | Empty -> list2


let set list =
    let rec SetOfMyListSub list st =
        match list with
        | Empty -> Set.empty
        | Cons (head, tail) -> st + Set.empty.Add(head) + SetOfMyListSub tail st

    SetOfMyListSub list Set.empty

let length list =
    let rec lengthSub list n =
        match list with
        | Empty -> n
        | Cons (head, tail) -> lengthSub tail (n + 1)
    lengthSub list 0
