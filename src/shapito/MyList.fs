module DoMyHomework.MyList

open DoMyHomework.OOPList

type MyList<'value> =
    | Cons of head: 'value * tail: MyList<'value>
    | Empty


/// That function insert second list to the tail of first
let rec concat (list1: MyList<'value>) (list2: MyList<'value>) : MyList<'value> =
    match list1 with
    | Cons (head, tail) -> Cons(head, concat tail list2)
    | Empty -> list2


/// This function takes a Mylist and sorts it using bubblesort
let rec bubbleSort (list: MyList<'value>) : MyList<'value> =

    /// This function swap the next two elements if
    /// the first element is greater than second
    let swap: MyList<'a> -> MyList<'a> =
        function
        | Cons (head1, Cons (head2, tail)) ->
            if head1 > head2 then
                Cons(head2, Cons(head1, tail))
            else
                Cons(head1, Cons(head2, tail))
        | Cons (head1, Empty) -> Cons(head1, Empty)
        | Empty -> Empty

    /// This function passes through all elements and applies swap to all on the way
    let rec passage (list: MyList<'value>) : MyList<'value> =
        match list with
        | Cons (head, tail) -> swap (Cons(head, passage tail))
        | Empty -> Empty

    /// This function checks if list is sorted
    let rec isSorted (list: MyList<'a>) : bool =
        match list with
        | Cons (value1, Cons (value2, tail)) ->
            if value1 <= value2 then
                isSorted (Cons(value2, tail))
            else
                false
        | Cons (_, Empty) -> true
        | Empty -> true

    // The cycle happens here
    if isSorted list then
        list
    else
        bubbleSort (passage list)



/// This function takes a Mylist and sorts it using quicksort
let rec quickSort (list: MyList<'a>) : MyList<'a> =

    /// That function divides the array into three parts:
    /// elements that are more than pivot
    /// less than pivot
    /// and equal to pivot.
    /// Then it applies quicksort to each part and concatenates them
    let rec divideAndApplyQuickSort list less equal more pivot =

        // If there is elements in list we divide them into groups
        match list with
        | Cons (head, tail) ->
            if head > pivot then
                divideAndApplyQuickSort tail less equal (Cons (head, more)) pivot
            elif head < pivot then
                divideAndApplyQuickSort tail (Cons (head, less)) equal more pivot
            else
                divideAndApplyQuickSort tail less (Cons (head, equal)) more pivot

        // If all elements divided into groups then we sorting each group and
        // concatenates them
        | Empty -> concat (concat (quickSort less) equal) (quickSort more)

    // That part is responsible for choosing the pivot
    // and calling divideAndApplyQuickSort with Empty values for less more and equal
    match list with
    | Empty -> Empty
    | Cons (head, tail) -> divideAndApplyQuickSort (Cons(head, tail)) Empty Empty Empty head
