namespace shapito

module Main =
    open OOPList
    open MyList
    open Translator

    [<EntryPoint>]
    let main argv =

        let a = [1; 2; 3; 4]

        let b = a @ [24; 53] @ [43; 2]

        printf "%A" b

        let lst = [1; 2; 3]

        let result = (OOPList.concat (lst |> listToOOPList) ([] |> listToOOPList)) =  (OOPList.concat ([] |> listToOOPList) (lst |> listToOOPList))

        0
