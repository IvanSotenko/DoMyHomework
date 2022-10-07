namespace shapito

module Main =
    open OOPList
    [<EntryPoint>]
    let main argv =

        let m = Translator.fromOOPListToMyList (concatenation (NonEmptyList(2, NonEmptyList(4, EmptyList()))) (NonEmptyList(2, NonEmptyList(4, EmptyList()))))
        printfn "%A" m

        0
