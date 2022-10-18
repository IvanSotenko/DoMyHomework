namespace DoMyHomework

open DoMyHomework.OOPList
open Convertor

module Main =

    [<EntryPoint>]
    let main argv =

        let a = [3.524245245; 3.62354256; 3.273643463465; 3.231643463; 3.6234523461376]
        let b = [0; -0; 0; -0; 0; -0]
        let c = [nan; 4.]
        let res = c |> ListToMyList |> MyList.quickSort //|> MyListToList

        let m = [nan]
        printfn $"{List.length m}"

        let t = nan
        // let res2 = List.sort c
        printfn $"{res}"
        0
