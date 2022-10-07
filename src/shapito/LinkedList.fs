module LinkedList

type MyList<'value> =
    | Cons of head: 'value * tail: MyList<'value>
    | Empty

let rec map f lst =
    match lst with
    | Empty -> Empty
    | Cons (hd, tl) -> Cons (f hd, map f tl)

let go () =
    map ((+)1) (Cons (1,Cons(3,Empty)))

let _go () =
    map ((-)1) (Cons (1,Cons(3,Empty)))


/// That function insert second list to the tail of first
let rec concatenation (list1: MyList<'value>) (list2: MyList<'value>) =
    match list1 with
    | Cons (head, tail) -> Cons (head, concatenation tail list2)
    | Empty -> list2


/// This function takes a Mylist and sorts it using bubblesort
let rec bubbleSort (list: MyList<'value>) =

    /// This function swap the next two elements if
    /// the first element is greater than second
    let  swap  = function
        | Cons (head1, Cons (head2, tail)) ->
            if head1 > head2 then
                Cons (head2, Cons (head1, tail))
            else
                Cons (head1, Cons (head2, tail))
        | Cons (head1, Empty) -> Cons (head1, Empty)
        | Empty -> Empty

    /// This function passes through all elements and applies swap to all on the way
    let rec passage (list: MyList<'value>) =
        match list with
        | Cons (head, tail) -> swap (Cons (head, passage tail))
        | Empty -> Empty

    /// This function checks if list is sorted
    let rec isSorted list =
        match list with
        | Cons (value1, Cons (value2, tail)) ->
            if value1 <= value2 then
                isSorted (Cons (value2, tail))
            else
                false
        | Cons (_, Empty) -> true
        | Empty -> true

    // The cycle happens here
    if isSorted list then
        list
    else
        bubbleSort (passage list)

// Converting Mylist to array
let rec MylistToArr arr (list: MyList<'value>) =
    match list with
    | Cons (head, tail) -> MylistToArr (Array.append arr [|head|]) tail
    | Empty -> arr

// Converting array to Mylist
let rec arrToMyList (i: int) (arr: int array) =
    if arr.Length <> i then
        // This dot is not redundant!
        Cons (arr.[i], (arrToMyList (i + 1) arr))
    else
        Empty

// This function takes a Mylist and sorts it using quicksort
let rec quickSort list =

    /// That function divides the array into three parts:
    /// elements that are more than pivot
    /// less than pivot
    /// and equal to pivot.
    /// Then it applies quicksort to each part and concatenates them
    let rec separator list less equal more pivot =

        // If there is elements in list we divide them into groups
        match list with
        | Cons (head, tail) ->
            if head < pivot then
                separator tail (concatenation less (Cons (head, Empty))) equal more pivot
            elif head = pivot then
                separator tail less (concatenation equal (Cons (head, Empty))) more pivot
            else
                separator tail less equal (concatenation more (Cons (head, Empty))) pivot

        // If all elements divided into groups then we sorting each group and
        // concatenates them
        | Empty -> concatenation (concatenation (quickSort less) equal) (quickSort more)

    // That part is responsible for choosing the pivot
    // and calling separator with Empty values for less more and equal
    match list with
    | Empty -> Empty
    | Cons (head, tail) -> separator (Cons (head, tail)) Empty Empty Empty head
