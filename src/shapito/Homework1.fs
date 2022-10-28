module DoMyHomework.Homework1

// (1) Exponentiation to integer in a simple way using mutable variables
let sillyPow (bas: float) (power: int) =

    let subSillyPow (num: float) (power: int) =

        let mutable output = 1.
        let mutable mutPower = power

        while mutPower > 0 do
            output <- output * num
            mutPower <- mutPower - 1

        output

    if power < 0 then
        1. / subSillyPow bas (abs power)
    else
        subSillyPow bas power


// (1) Exponentiation to natural and zero power in a simple way by recursion
let SillyPowRec (bas: float) (power: int) =

    let rec subSillyPowRec (bas: float) (power: int) =

        if power = 0 then
            1.
        elif power = 1 then
            bas
        else
            bas * subSillyPowRec bas (power - 1)

    if power < 0 then
        1. / subSillyPowRec bas (abs power)
    else
        subSillyPowRec bas power


// (2) Fast power
let rec fastPow (bas: float) (power: int) =

    let rec subFastPow (bas: float) (power: int) =
        if power = 0 then
            1.
        elif power % 2 = 0 then
            fastPow (bas * bas) (power / 2)
        else
            bas * fastPow bas (power - 1)

    if power < 0 then
        1. / subFastPow bas (abs power)
    else
        subFastPow bas power


// (3) The difference between the largest and smallest element of the array
let delta arr =
    if Array.isEmpty arr then
        failwith "Error in Homework1.delta: empty array was given. Doesn't make sense"

    else
        let mutable min = arr[0]
        let mutable max = arr[0]

        for element in arr do
            if element > max then max <- element
            elif element < min then min <- element

        max - min


// (4) An array of odd numbers
let oddNumbersBetween (num1: int) (num2: int) =

    let (left_edge: int) =
        if abs (min num1 num2) % 2 = 0 then
            (min num1 num2) + 1
        else
            (min num1 num2) + 2

    let (right_edge: int) =
        if abs (max num1 num2) % 2 = 0 then
            (max num1 num2) - 1
        else
            (max num1 num2) - 2

    [| for i in left_edge..2..right_edge -> i |]
