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
