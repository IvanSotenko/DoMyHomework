namespace shapito.Tests

module ExpectoTemplate =

    open Expecto

    [<EntryPoint>]
    let main argv =

        infinity ** -3

        Tests.runTestsInAssembly defaultConfig argv
