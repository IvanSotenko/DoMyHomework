module MyList

type MyList<'value> =
    | Cons of head: 'value * tail: MyList<'value>
    | Empty
