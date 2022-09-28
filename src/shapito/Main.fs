namespace shapito

open System.Reflection

module AssemblyInfo =

    let metaDataValue (mda: AssemblyMetadataAttribute) = mda.Value

    let getMetaDataAttribute (assembly: Assembly) key =
        assembly.GetCustomAttributes(typedefof<AssemblyMetadataAttribute>)
        |> Seq.cast<AssemblyMetadataAttribute>
        |> Seq.find (fun x -> x.Key = key)

    let getReleaseDate assembly =
        "ReleaseDate"
        |> getMetaDataAttribute assembly
        |> metaDataValue

    let getGitHash assembly =
        "GitHash"
        |> getMetaDataAttribute assembly
        |> metaDataValue

    let getVersion assembly =
        "AssemblyVersion"
        |> getMetaDataAttribute assembly
        |> metaDataValue

    let assembly = lazy Assembly.GetEntryAssembly()

    let printVersion () =
        let version = assembly.Force().GetName().Version

        printfn "%A" version

    let printInfo () =
        let assembly = assembly.Force()
        let name = assembly.GetName()
        let version = assembly.GetName().Version
        let releaseDate = getReleaseDate assembly
        let githash = getGitHash assembly
        printfn "%s - %A - %s - %s" name.Name version releaseDate githash

module Say =
    open System

    let nothing name = name |> ignore

    let hello name = sprintf "Hello %s" name

    let colorizeIn (color: string) str =
        let oldColor = Console.ForegroundColor
        Console.ForegroundColor <- (Enum.Parse(typedefof<ConsoleColor>, color) :?> ConsoleColor)
        printfn "%s" str
        Console.ForegroundColor <- oldColor

module FirstHw =

    // (1) Exponentiation of ints to int power in a simple way
    let silly_pow (bas: int) (power: int) =

        let pow num power =

            let mutable output = 1
            let mutable mut_power = power

            while (mut_power > 0) do
                output <- output * num
                mut_power <- mut_power - 1

            output

        if power > 0 then
            float (pow bas power)

        elif power < 0 then
            float 1 / float (pow bas (-power))

        else
            float 1

    // (1) Exponentiation to natural and zero power in a simple way by recursion
    let rec silly_pow_rec (bas: int) (power: int) =

        if power = 0 then 1
        elif power = 1 then bas
        else bas * silly_pow_rec bas (power - 1)

    // (2) Fast power
    let rec fast_pow (bas: uint) (power: uint) =
        if power = 0u then
            1u
        elif power % 2u = 0u then
            fast_pow (bas * bas) (power / 2u)
        else
            bas * fast_pow bas (power - 1u)

    // (3) The difference between the largest and smallest element of the array
    let delta (arr: int array) =
        if arr = [||] then
            0
        else
            let mutable min = arr[0]
            let mutable max = arr[0]

            for element in arr do
                if element > max then max <- element
                elif element < min then min <- element

            max - min

    // (4) An array of odd numbers
    let odd_numbers_between (num1: int) (num2: int) =

        let (right_edge: int) =
            match abs (max num1 num2) % 2 with
            | 0 -> (max num1 num2) - 1
            | 1 -> (max num1 num2) - 2
            | _ -> failwith "How? -_-"

        let (left_edge: int) =
            match abs (min num1 num2) % 2 with
            | 0 -> (min num1 num2) + 1
            | 1 -> (min num1 num2) + 2
            | _ -> failwith "How? -_-"

        // Could do it like this:
        // let (left_edge: int) = if abs (min num1 num2) % 2 = 0 then (min num1 num2) + 1 else (min num1 num2) + 2
        // let (right_edge: int) = if abs (min num1 num2) % 2 = 0 then (max num1 num2) - 1 else (max num1 num2) - 2

        [| for i in left_edge..2..right_edge -> i |]

module Main =
    open Argu

    type CLIArguments =
        | Info
        | Version
        | Favorite_Color of string // Look in App.config
        | [<MainCommand>] Hello of string
        interface IArgParserTemplate with
            member s.Usage =
                match s with
                | Info -> "More detailed information"
                | Version -> "Version of application"
                | Favorite_Color _ -> "Favorite color"
                | Hello _ -> "Who to say hello to"

    [<EntryPoint>]
    let main (argv: string array) =
        let parser = ArgumentParser.Create<CLIArguments>(programName = "shapito")

        let results = parser.Parse(argv)

        if results.Contains Version then
            AssemblyInfo.printVersion ()
        elif results.Contains Info then
            AssemblyInfo.printInfo ()
        elif results.Contains Hello then
            match results.TryGetResult Hello with
            | Some v ->
                let color = results.GetResult Favorite_Color
                Say.hello v |> Say.colorizeIn color
            | None -> parser.PrintUsage() |> printfn "%s"
        else
            parser.PrintUsage() |> printfn "%s"

        0
