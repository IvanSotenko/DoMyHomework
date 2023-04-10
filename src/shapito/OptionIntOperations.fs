module DoMyHomework.OptionIntOperations

let addInt (a: Option<int>) (b: Option<int>) =
    match a, b with
    | Some x, Some y -> Some(x + y)
    | Some x, None
    | None, Some x -> Some x
    | None, None -> None

let multInt (a: Option<int>) (b: Option<int>) =
    match a, b with
    | Some x, Some y -> Some(x * y)
    | _ -> None
