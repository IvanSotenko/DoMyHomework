namespace shapito

module FirstHw =

    // (1) Exponentiation of ints to int power in a simple way
    let silly_pow (bas: float) (power: int) =

        let pow (num: float) (power: int) =

            let mutable output = 1.
            let mutable mut_power = power

            while mut_power > 0 do
                output <- output * num
                mut_power <- mut_power - 1

            output

        if power > 0 then
            pow bas power

        elif power < 0 then
            1. / pow bas (-power)

        else
            1.

    // (1) Exponentiation to natural and zero power in a simple way by recursion
    let rec silly_pow_rec (bas: float) (power: uint) =

        if power = 0u then
            1.
        elif power = 1u then
            bas
        else
            bas * silly_pow_rec bas (power - 1u)

    // (2) Fast power
    let rec fast_pow (bas: float) (power: uint) =
        if power = 0u then
            1.
        elif power % 2u = 0u then
            fast_pow (bas * bas) (power / 2u)
        else
            bas * fast_pow bas (power - 1u)

    // (3) The difference between the largest and smallest element of the array
    let delta arr =
        if arr = [||] then
            failwith "Error in Main.fs/FirstHw/delta: empty array was given. Doesn't make sense"

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

module Main =

    [<EntryPoint>]
    let main argv =
        0
