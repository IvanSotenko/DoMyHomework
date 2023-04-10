namespace DoMyHomework.Tests

module testRunner =

    open Expecto

    [<EntryPoint>]
    let main argv =
        runTestsInAssembly defaultConfig argv |> ignore
        0
