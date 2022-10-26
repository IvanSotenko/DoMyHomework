module DoMyHomework.OOPList

type IList<'Value> =
    interface
    end

type NonEmptyList<'Value>(head: 'Value, tail: IList<'Value>) =
    interface IList<'Value>
    member this.Head = head
    member this.Tail = tail

type EmptyList<'Value>() =
    interface IList<'Value>

/// That function insert second list to the tail of first.
/// So it concatenates them
let rec concat (list1: IList<'Value>) (list2: IList<'Value>) : IList<'Value> =
    match list1 with
    | :? EmptyList<'Value> -> list2
    | :? NonEmptyList<'Value> as lst -> NonEmptyList(lst.Head, concat lst.Tail list2)
    | _ ->
        failwith
            $"OOPList.concat: the input data type was expected to be \
                    OOPList+NonEmptyList or OOPList+EmptyList, \
                    but {(list1.GetType())} was given"


/// This function takes a OOPList and returns its head
let getHead (lst: IList<'Value>) : 'Value =
    match lst with
    | :? NonEmptyList<'Value> as list -> list.Head
    | _ ->
        failwith
            $"OOPList.getHead: the input data type was expected to be \
                    OOPList+NonEmptyList, but {(lst.GetType())} was given"

/// This function takes a OOPList and returns its Tail
let getTail (lst: IList<'Value>) : IList<'Value> =
    match lst with
    | :? NonEmptyList<'Value> as list -> list.Tail
    | _ ->
        failwith
            $"OOPList.getTail: the input data type was expected to be \
                    OOPList+NonEmptyList, but {(lst.GetType())} was given"



/// This function takes a MyOOPlist and sorts it using bubblesort,
/// works the same way as a similar function for MyList
let rec bubbleSort (list: IList<'Value>) : IList<'Value> =

    /// This function swap the next two elements if
    /// the first element is greater than second
    let swap (lst: IList<'Value>) : IList<'Value> =
        match lst with
        | :? EmptyList<'Value> -> EmptyList() :> IList<'Value>
        | :? NonEmptyList<'Value> as list ->
            if list.Tail :? NonEmptyList<'Value> then
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
    let rec passage (lst: IList<'Value>) : IList<'Value> =
        match lst with
        | :? EmptyList<'Value> -> EmptyList() :> IList<'Value>
        | :? NonEmptyList<'Value> as list -> swap (NonEmptyList(list.Head, (passage list.Tail)))
        | _ ->
            failwith
                $"OOPList.bubbleSort.passage: the input data type was expected to be \
                        OOPList+NonEmptyList or OOPList+EmptyList, \
                        but {(lst.GetType())} was given"

    /// This function checks if list is sorted
    let rec isSorted (lst: IList<'Value>) : bool =
        match lst with
        | :? EmptyList<'Value> -> true
        | :? NonEmptyList<'Value> as list ->
            list.Tail :? EmptyList<'Value>
            || list.Head <= getHead list.Tail
               && isSorted list.Tail
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
let rec quickSort (lst: IList<'Value>) : IList<'Value> =

    /// That function divides the array into three parts:
    /// elements that are more than pivot
    /// less than pivot
    /// and equal to pivot.
    let rec split
        (lst: IList<'Value>)
        (less: IList<'Value>)
        (equal: IList<'Value>)
        (more: IList<'Value>)
        (pivot: 'Value)
        =

        match lst with

        | :? EmptyList<'Value> -> less, equal, more

        | :? NonEmptyList<'Value> as list ->
            if list.Head < pivot then
                split list.Tail (NonEmptyList(list.Head, less)) equal more pivot
            elif list.Head = pivot then
                split list.Tail less (NonEmptyList(list.Head, equal)) more pivot
            else
                split list.Tail less equal (NonEmptyList(list.Head, more)) pivot


        | _ ->
            failwith
                $"OOPList.quickSort.divideAndApplyQuickSort: the input data type was expected to be \
                        OOPList+NonEmptyList or OOPList+EmptyList, \
                        but {(lst.GetType())} was given"

    match lst with
    | :? EmptyList<'Value> -> EmptyList() :> IList<'Value>
    | :? NonEmptyList<'Value> as list ->

        // Divide the list into several parts
        let parts = split list (EmptyList()) (EmptyList()) (EmptyList()) list.Head

        // Sort less and more, then concatenate everything together
        match parts with
        | less, equal, more -> concat (concat (quickSort less) equal) (quickSort more)

    | _ ->
        failwith
            $"OOPList.quickSort: the input data type was expected to be \
                    OOPList+NonEmptyList or OOPList+EmptyList, \
                    but {(lst.GetType())} was given"
