namespace shapito

module Main =
    open OOPList
    [<EntryPoint>]
    let main argv =

        let m = Translator.fromOOPListToMyList (concatenation (NonEmptyList(2, NonEmptyList(4, EmptyList()))) (NonEmptyList(2, NonEmptyList(4, EmptyList()))))
        printfn "%A" m

        let k = Translator.fromOOPListToMyList (bubbleSort (NonEmptyList(6, NonEmptyList(3, NonEmptyList(0, NonEmptyList(2, NonEmptyList(3, EmptyList())))))))
        printfn "%A" k

        0
