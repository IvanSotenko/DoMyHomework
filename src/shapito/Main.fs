namespace DoMyHomework

open DoMyHomework.OOPList

module Main =

    [<EntryPoint>]
    let main argv =

        let a = NonEmptyList (4, NonEmptyList (5, EmptyList()))
        // let c = Something 5

        // let b = concat c a
        // printfn "%A" (c.GetType())
        let b = [|"3"; "4"|]
        printf $"{b[1]}"
        0
