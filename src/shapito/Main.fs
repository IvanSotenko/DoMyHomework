namespace shapito

module Main =
    open OOPList
    [<EntryPoint>]
    let main argv =

        let m = Translator.fromOOPListToMyList (concatenation (NonEmptyList(2, NonEmptyList(4, EmptyList()))) (NonEmptyList(2, NonEmptyList(4, EmptyList()))))
        printfn "%A" m

        let a = NonEmptyList(3, NonEmptyList(-1, EmptyList()))
        printfn "%A" (Translator.fromOOPListToMyList (swap a))

        0
