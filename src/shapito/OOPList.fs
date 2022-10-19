module DoMyHomework.OOPList

type IList<'value> =
    interface
    end

//[<AllowNullLiteral>]
type NonEmptyList<'value>(head: 'value, tail: IList<'value>) =
    interface IList<'value>
    member this.Head = head
    member this.Tail = tail

type EmptyList<'value>() =
    interface IList<'value>

/// That function insert second list to the tail of first.
/// So it concatenates them
let rec concat (list1: IList<'value>) (list2: IList<'value>) : IList<'value> =
    match list1 with
    | :? EmptyList<'value> -> list2
    | :? NonEmptyList<'value> as lst -> NonEmptyList(lst.Head, concat lst.Tail list2)
    | _ ->
        failwith
            $"OOPList.concat: the input data type was expected to be \
                    OOPList+NonEmptyList or OOPList+EmptyList, \
                    but {(list1.GetType())} was given"


/// This function takes a OOPList and returns its head
let getHead (lst: IList<'value>) : 'value =
    match lst with
    | :? NonEmptyList<'value> as list -> list.Head
    | _ ->
        failwith
            $"OOPList.getHead: the input data type was expected to be \
                    OOPList+NonEmptyList, but {(lst.GetType())} was given"

/// This function takes a OOPList and returns its Tail
let getTail (lst: IList<'value>) : IList<'value> =
    match lst with
    | :? NonEmptyList<'value> as list -> list.Tail
    | _ ->
        failwith
            $"OOPList.getTail: the input data type was expected to be \
                    OOPList+NonEmptyList, but {(lst.GetType())} was given"



/// This function takes a MyOOPlist and sorts it using bubblesort,
/// works the same way as a similar function for MyList
let rec bubbleSort (list: IList<'value>) : IList<'value> =

    /// This function swap the next two elements if
    /// the first element is greater than second
    let swap (lst: IList<'value>) : IList<'value> =
        match lst with
        | :? EmptyList<'value> -> EmptyList() :> IList<'value>
        | :? NonEmptyList<'value> as list ->
            if list.Tail :? NonEmptyList<'value> then
                if list.Head > getHead list.Tail then
                    NonEmptyList(getHead list.Tail, NonEmptyList(list.Head, getTail list.Tail))
                else
                    NonEmptyList(list.Head, list.Tail)
            else
                NonEmptyList(list.Head, EmptyList())
        | _ ->
            failwith
                $"OOPList.bubbleSort.swap: the input data type was expected to be \
                        OOPList+NonEmptyList or OOPList+EmptyList, \
                        but {(lst.GetType())} was given"

    /// This function passes through all elements and applies swap to all on the way
    let rec passage (lst: IList<'value>) : IList<'value> =
        match lst with
        | :? EmptyList<'value> -> EmptyList() :> IList<'value>
        | :? NonEmptyList<'value> as list -> swap (NonEmptyList(list.Head, (passage list.Tail)))
        | _ ->
            failwith
                $"OOPList.bubbleSort.passage: the input data type was expected to be \
                        OOPList+NonEmptyList or OOPList+EmptyList, \
                        but {(lst.GetType())} was given"

    /// This function checks if list is sorted
    let rec isSorted (lst: IList<'value>) : bool =
        match lst with
        | :? EmptyList<'value> -> true
        | :? NonEmptyList<'value> as list ->
            if list.Tail :? NonEmptyList<'value> then
                if list.Head <= getHead list.Tail then
                    isSorted list.Tail
                else
                    false
            else
                true
        | _ ->
            failwith
                $"OOPList.bubbleSort.isSorted: the input data type was expected to be \
                        OOPList+NonEmptyList or OOPList+EmptyList, \
                        but {(lst.GetType())} was given"

    // The cycle happens here
    if isSorted list then
        list
    else
        bubbleSort (passage list)


/// This function takes a OOPList and sorts it using quicksort
/// works the same way as one for Mylist
let rec quickSort (lst: IList<'value>) : IList<'value> =

    /// That function divides the array into three parts:
    /// elements that are more than pivot
    /// less than pivot
    /// and equal to pivot.
    /// Then it applies quicksort to each part and concatenates them
    let rec divideAndApplyQuickSort
        (lst: IList<'value>)
        (less: IList<'value>)
        (equal: IList<'value>)
        (more: IList<'value>)
        (pivot: 'value)
        : IList<'value> =

        match lst with

        // If all elements divided into groups then we sorting each group and
        // concatenates them
        | :? EmptyList<'value> -> concat (concat (quickSort less) equal) (quickSort more)

        // If there is elements in list we divide them into groups
        | :? NonEmptyList<'value> as list ->
            if list.Head < pivot then
                // divideAndApplyQuickSort list.Tail (concat less (NonEmptyList(list.Head, EmptyList()))) equal more pivot
                divideAndApplyQuickSort list.Tail (NonEmptyList(list.Head, less)) equal more pivot
            elif list.Head = pivot then
                divideAndApplyQuickSort list.Tail less (NonEmptyList(list.Head, equal)) more pivot
            else
                divideAndApplyQuickSort list.Tail less equal (NonEmptyList(list.Head, more)) pivot
        | _ ->
            failwith
                $"OOPList.quickSort.divideAndApplyQuickSort: the input data type was expected to be \
                        OOPList+NonEmptyList or OOPList+EmptyList, \
                        but {(lst.GetType())} was given"

    // That part is responsible for choosing the pivot
    // and calling divideAndApplyQuickSort with Empty values for less more and equal
    match lst with
    | :? EmptyList<'value> -> EmptyList() :> IList<'value>
    | :? NonEmptyList<'value> as list ->
        divideAndApplyQuickSort list (EmptyList()) (EmptyList()) (EmptyList()) list.Head
    | _ ->
        failwith
            $"OOPList.quickSort: the input data type was expected to be \
                    OOPList+NonEmptyList or OOPList+EmptyList, \
                    but {(lst.GetType())} was given"
