module DoMyHomework.MyList

open System.Globalization

type MyList<'Value> =
    | Cons of head: 'Value * tail: MyList<'Value>
    | Empty


/// That function insert second list to the tail of first
let rec concat (list1: MyList<'Value>) (list2: MyList<'Value>) : MyList<'Value> =
    match list1 with
    | Cons (head, tail) -> Cons(head, concat tail list2)
    | Empty -> list2


/// This function takes a Mylist and sorts it using bubblesort
let rec bubbleSort (list: MyList<'Value>) : MyList<'Value> =

    /// This function swap the next two elements if
    /// the first element is greater than second
    let swap: MyList<'A> -> MyList<'A> =
        function
        | Cons (head1, Cons (head2, tail)) ->
            if head1 > head2 then
                Cons(head2, Cons(head1, tail))
            else
                Cons(head1, Cons(head2, tail))
        | Cons (head1, Empty) -> Cons(head1, Empty)
        | Empty -> Empty

    /// This function passes through all elements and applies swap to all on the way
    let rec passage (list: MyList<'Value>) : MyList<'Value> =
        match list with
        | Cons (head, tail) -> swap (Cons(head, passage tail))
        | Empty -> Empty

    /// This function checks if list is sorted
    let rec isSorted (list: MyList<'A>) : bool =
        match list with
        | Cons (value1, Cons (value2, tail)) -> value1 <= value2 && isSorted (Cons(value2, tail))
        | Cons (_, Empty) -> true
        | Empty -> true

    // The cycle happens here
    if isSorted list then
        list
    else
        bubbleSort (passage list)



/// This function takes a Mylist and sorts it using quicksort
let rec quickSort (list: MyList<'A>) : MyList<'A> =

    /// That function divides the array into three parts:
    /// elements that are more than pivot
    /// less than pivot
    /// and equal to pivot.
    let rec split list less equal more pivot =

        match list with
        | Cons (head, tail) ->
            if head > pivot then
                split tail less equal (Cons(head, more)) pivot
            elif head < pivot then
                split tail (Cons(head, less)) equal more pivot
            else
                split tail less (Cons(head, equal)) more pivot

        | Empty -> less, equal, more

    match list with
    | Empty -> Empty
    | Cons (head, _) ->

        // Divide the list into several parts
        let parts = split list Empty Empty Empty head

        // Sort less and more, then concatenate everything together
        match parts with
        | less, equal, more -> concat (concat (quickSort less) equal) (quickSort more)

let toSet (list: MyList<'A>) : Set<'A> =
    let rec toSetSub list st =
        match list with
        | Empty -> Set.empty
        | Cons (head, tail) -> st + Set.empty.Add(head) + toSetSub tail st

    toSetSub list Set.empty


let length (list: MyList<'A>) : int =
    let rec lengthSub list n =
        match list with
        | Empty -> n
        | Cons (_, tail) -> lengthSub tail (n + 1)

    lengthSub list 0

let fold (folder: 'State -> 'A -> 'State) (state: 'State) (list: MyList<'A>) : 'State =
    let rec foldSub acc list =
        match list with
        | Cons (head, tail) -> foldSub (folder acc head) tail
        | Empty -> acc

    foldSub state list

let foldRev (folder: 'State -> 'A -> 'State) (state: 'State) (list: MyList<'A>) : 'State =
    let rec foldSub acc list =
        match list with
        | Cons (head, tail) -> folder (foldSub acc tail) head
        | Empty -> acc

    foldSub state list
