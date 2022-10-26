module shapito.Homework1

// (1) Exponentiation to integer in a simple way using mutable variables
let silly_pow (bas: float) (power: int) =

    let silly_pow_sub (num: float) (power: int) =

        let mutable output = 1.
        let mutable mut_power = power

        while mut_power > 0 do
            output <- output * num
            mut_power <- mut_power - 1

        output

    if power < 0 then
        1. / silly_pow_sub bas (abs power)
    else
        silly_pow_sub bas power


// (1) Exponentiation to natural and zero power in a simple way by recursion
let silly_pow_rec (bas: float) (power: int) =

    let rec silly_pow_rec_sub (bas: float) (power: int) =

        if power = 0 then
            1.
        elif power = 1 then
            bas
        else
            bas * silly_pow_rec_sub bas (power - 1)

    if power < 0 then
        1. / silly_pow_rec_sub bas (abs power)
    else
        silly_pow_rec_sub bas power


// (2) Fast power
let rec fast_pow (bas: float) (power: int) =

    let rec fast_pow_sub (bas: float) (power: int) =
        if power = 0 then
            1.
        elif power % 2 = 0 then
            fast_pow (bas * bas) (power / 2)
        else
            bas * fast_pow bas (power - 1)

    if power < 0 then
        1. / fast_pow_sub bas (abs power)
    else
        fast_pow_sub bas power


// (3) The difference between the largest and smallest element of the array
let delta arr =
    if arr = [||] then
        failwith "Error in Homework1.delta: empty array was given. Doesn't make sense"

    else
        let mutable min = arr[0]
        let mutable max = arr[0]

        for element in arr do
            if element > max then max <- element
            elif element < min then min <- element

        max - min


// (4) An array of odd numbers
let odd_numbers_between (num1: int) (num2: int) =

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
