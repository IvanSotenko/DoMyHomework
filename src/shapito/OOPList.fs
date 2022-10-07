module shapito.OOPList

type IList<'value> = interface end

//[<AllowNullLiteral>]
type MyOOPNonEmptyList<'value> (head: 'value, tail: IList<'value>) =
    interface IList<'value>
    member this.Head = head
    member this.Tail = tail

type MyOOPEmptyList<'value>() =
    interface IList<'value>

type IActor<'inType, 'outType> =
    abstract Do: 'inType -> 'outType

let rec oopMap (f:IActor<'value,'result>) (lst:IList<'value>) =
    if lst :? MyOOPEmptyList<'value>
    then MyOOPEmptyList() :> IList<'result>
    elif lst :? MyOOPNonEmptyList<'value>
    then
        let lst = lst :?> MyOOPNonEmptyList<'value>
        MyOOPNonEmptyList(f.Do lst.Head, oopMap f lst.Tail)
    else failwith "!!!"

let rec oopMap2 (f:IActor<'value,'result>) (lst:IList<'value>) =
    match lst with
    | :? MyOOPEmptyList<'value> ->
        MyOOPEmptyList () :> IList<'result>
    | :? MyOOPNonEmptyList<'value> as lst ->
        MyOOPNonEmptyList(f.Do lst.Head, oopMap f lst.Tail)


type PlusOneActor () =
    interface IActor<int,int> with
        member this.Do x = x + 1

type MinusOneActor () =
    interface IActor<int,int> with
        member this.Do x = x - 1

let _go2() =
    let lst = MyOOPNonEmptyList(1,MyOOPNonEmptyList(3,MyOOPEmptyList()))
    oopMap (PlusOneActor()) lst

let go2() =
    let lst = MyOOPNonEmptyList(1,MyOOPNonEmptyList(3,MyOOPEmptyList()))
    oopMap (MinusOneActor()) lst
