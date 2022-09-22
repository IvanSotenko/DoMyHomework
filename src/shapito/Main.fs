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

    let assembly = lazy (Assembly.GetEntryAssembly())

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

module First_hw =
    let silly_pow num power =

        let pow num power =

            let mutable output = 1
            let mutable mut_power = power

            while (mut_power > 0) do
                output <- output * num
                mut_power <- mut_power - 1

            output

        if power > 0 then
            float (pow num power)

        elif power < 0 then
            float 1f / float (pow num (-power))

        else
            float(1)

// Самая простая функция возведения в степень, принимает только натуральные показатели степени и ноль
    let rec silly_pow_rec (bas: int) (power: int) =

        if power = 0
        then 1
        elif power = 1
        then bas
        else
            bas * silly_pow_rec bas (power - 1)

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

        printfn $"res: %A{First_hw.silly_pow 2 -3}"

        0
