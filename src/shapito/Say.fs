module shapito.Say

module Say =
    open System

    let nothing name = name |> ignore

    let hello name = sprintf "Hello %s" name

    let colorizeIn (color: string) str =
        let oldColor = Console.ForegroundColor
        Console.ForegroundColor <- (Enum.Parse(typedefof<ConsoleColor>, color) :?> ConsoleColor)
        printfn "%s" str
        Console.ForegroundColor <- oldColor
