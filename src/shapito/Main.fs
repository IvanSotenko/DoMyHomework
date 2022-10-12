namespace shapito

module Main =
    open OOPList
    open MyList

    [<EntryPoint>]
    let main argv =

        let a = [1; 2; 3; 4]

        let b = a @ [24; 53] @ [43; 2]

        printf "%A" b

        0
