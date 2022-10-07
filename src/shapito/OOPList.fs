module shapito.OOPList

type IList<'value> = interface end

//[<AllowNullLiteral>]
type NonEmptyList<'value> (head: 'value, tail: IList<'value>) =
    interface IList<'value>
    member this.Head = head
    member this.Tail = tail

type EmptyList<'value>() =
    interface IList<'value>

type IActor<'inType, 'outType> =
    abstract Do: 'inType -> 'outType

let rec oopMap (f:IActor<'value,'result>) (lst:IList<'value>) =
    if lst :? EmptyList<'value>
    then EmptyList() :> IList<'result>
    elif lst :? NonEmptyList<'value>
    then
        let lst = lst :?> NonEmptyList<'value>
        NonEmptyList(f.Do lst.Head, oopMap f lst.Tail)
    else failwith "!!!"

let rec oopMap2 (f:IActor<'value,'result>) (lst:IList<'value>) =
    match lst with
    | :? EmptyList<'value> ->
        EmptyList () :> IList<'result>
    | :? NonEmptyList<'value> as lst ->
        NonEmptyList(f.Do lst.Head, oopMap f lst.Tail)

type PlusOneActor () =
    interface IActor<int,int> with
        member this.Do x = x + 1

type MinusOneActor () =
    interface IActor<int,int> with
        member this.Do x = x - 1

let _go2() =
    let lst = NonEmptyList(1,NonEmptyList(3,EmptyList()))
    oopMap (PlusOneActor()) lst

let go2() =
    let lst = NonEmptyList(1,NonEmptyList(3,EmptyList()))
    oopMap (MinusOneActor()) lst

let rec concatenation (list1:IList<'value>) (list2:IList<'value>) =
    match list1 with
    | :? EmptyList<'value> -> list2
    | :? NonEmptyList<'value> as lst ->
        NonEmptyList( lst.Head, concatenation list2 lst.Tail)
    | _ -> failwith "fail in OOPList concatenation"

// let bubbleSort (list: IList<'value>) =

    // let  swap  = function
    //     | Cons (head1, Cons (head2, tail)) ->
    //         if head1 > head2 then
    //             Cons (head2, Cons (head1, tail))
    //         else
    //             Cons (head1, Cons (head2, tail))
    //     | Cons (head1, Empty) -> Cons (head1, Empty)
    //     | Empty -> Empty

let getHead (lst: IList<'value>) =
    match lst with
    | :? NonEmptyList<'value> as list -> list.Head
    | _ -> failwith "{working on...}"

let getTail (lst: IList<'value>) =
    match lst with
    | :? NonEmptyList<'value> as list -> list.Tail
    | _ -> failwith "{working on...}"

    /// This function swap the next two elements if
    /// the first element is greater than second
let swap (lst: IList<'value>) =
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

let rec passage (lst: IList<'value>) =
    match lst with
    | :? EmptyList<'value> -> EmptyList () :> IList<'value>
    | :? NonEmptyList<'value> as list -> swap (NonEmptyList (list.Head, (passage list.Tail)))
    | _ -> failwith "{working on...}"
