module shapito.OOPList

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

type IActor<'inType, 'outType> =
    abstract Do: 'inType -> 'outType

let rec oopMap (f: IActor<'value, 'result>) (lst: IList<'value>): IList<'result> =
    if lst :? EmptyList<'value> then
        EmptyList() :> IList<'result>
    elif lst :? NonEmptyList<'value> then
        let lst = lst :?> NonEmptyList<'value>
        NonEmptyList(f.Do lst.Head, oopMap f lst.Tail)
    else
        failwith "!!!"

let rec oopMap2 (f: IActor<'value, 'result>) (lst: IList<'value>): IList<'result> =
    match lst with
    | :? EmptyList<'value> -> EmptyList() :> IList<'result>
    | :? NonEmptyList<'value> as lst -> NonEmptyList(f.Do lst.Head, oopMap f lst.Tail)
    | _ -> failwith "{working on...}"

type PlusOneActor() =
    interface IActor<int, int> with
        member this.Do x = x + 1

type MinusOneActor() =
    interface IActor<int, int> with
        member this.Do x = x - 1

let _go2 (): IList<int> =
    let lst = NonEmptyList(1, NonEmptyList(3, EmptyList()))
    oopMap (PlusOneActor()) lst

let go2 (): IList<int> =
    let lst = NonEmptyList(1, NonEmptyList(3, EmptyList()))
    oopMap (MinusOneActor()) lst



/// That function insert second list to the tail of first.
/// So it concatenates them
let rec concat (list1: IList<'value>) (list2: IList<'value>): IList<'value> =
    match list1 with
    | :? EmptyList<'value> -> list2
    | :? NonEmptyList<'value> as lst -> NonEmptyList(lst.Head, concat lst.Tail list2)
    | _ -> failwith "fail in OOPList concatenation"


/// This function takes a OOPList and returns its head
let getHead (lst: IList<'value>): 'value =
    match lst with
    | :? NonEmptyList<'value> as list -> list.Head
    | _ -> failwith "{working on...}"

/// This function takes a OOPList and returns its Tail
let getTail (lst: IList<'value>): IList<'value> =
    match lst with
    | :? NonEmptyList<'value> as list -> list.Tail
    | _ -> failwith "{working on...}"



/// This function takes a MyOOPlist and sorts it using bubblesort,
/// works the same way as a similar function for MyList
let rec bubbleSort (list: IList<'value>): IList<'value> =

    /// This function swap the next two elements if
    /// the first element is greater than second
    let swap (lst: IList<'value>): IList<'value> =
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
        | _ -> failwith "{working on...}"

    /// This function passes through all elements and applies swap to all on the way
    let rec passage (lst: IList<'value>): IList<'value> =
        match lst with
        | :? EmptyList<'value> -> EmptyList() :> IList<'value>
        | :? NonEmptyList<'value> as list -> swap (NonEmptyList(list.Head, (passage list.Tail)))
        | _ -> failwith "{working on...}"

    /// This function checks if list is sorted
    let rec isSorted (lst: IList<'value>): bool =
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
        | _ -> failwith "{working on...}"

    // The cycle happens here
    if isSorted list then
        list
    else
        bubbleSort (passage list)


/// This function takes a OOPList and sorts it using quicksort
/// works the same way as one for Mylist
let rec quickSort (lst: IList<'value>): IList<'value> =

    /// That function divides the array into three parts:
    /// elements that are more than pivot
    /// less than pivot
    /// and equal to pivot.
    /// Then it applies quicksort to each part and concatenates them
    let rec separator (lst: IList<'value>) (less: IList<'value>) (equal: IList<'value>) (more: IList<'value>) (pivot: 'value): IList<'value> =

        match lst with

        // If all elements divided into groups then we sorting each group and
        // concatenates them
        | :? EmptyList<'value> -> concat (concat (quickSort less) equal) (quickSort more)

        // If there is elements in list we divide them into groups
        | :? NonEmptyList<'value> as list ->
            if list.Head < pivot then
                separator list.Tail (concat less (NonEmptyList(list.Head, EmptyList()))) equal more pivot
            elif list.Head = pivot then
                separator list.Tail less (concat equal (NonEmptyList(list.Head, EmptyList()))) more pivot
            else
                separator list.Tail less equal (concat more (NonEmptyList(list.Head, EmptyList()))) pivot
        | _ -> failwith "{working on...}"

    // That part is responsible for choosing the pivot
    // and calling separator with Empty values for less more and equal
    match lst with
    | :? EmptyList<'value> -> EmptyList() :> IList<'value>
    | :? NonEmptyList<'value> as list -> separator list (EmptyList()) (EmptyList()) (EmptyList()) list.Head
    | _ -> failwith "{working on...}"
