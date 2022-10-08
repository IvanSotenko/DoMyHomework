namespace shapito

module Main =
    open OOPList
    open MyList

    [<EntryPoint>]
    let main argv =

        let m =
            Translator.OOPListToMyList (
                OOPList.concatenation
                    (NonEmptyList(2, NonEmptyList(4, EmptyList())))
                    (NonEmptyList(2, NonEmptyList(4, EmptyList())))
            )

        printfn "%A" m

        let k =
            Translator.OOPListToMyList (
                OOPList.quickSort (
                    NonEmptyList(6, NonEmptyList(3, NonEmptyList(0, NonEmptyList(2, NonEmptyList(3, EmptyList())))))
                )
            )

        printfn "%A" k

        let b =
            Translator.MyListToArr (Cons (3, Cons (5, Cons (9, Cons (5, Cons (1, Empty))))))

        printfn "%A" b

        0
