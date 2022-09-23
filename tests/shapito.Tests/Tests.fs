namespace shapito.Tests

open Expecto
open shapito

module SayTests =
    [<Tests>]
    let tests =
        testList
            "samples"
            [ testCase "Say nothing"
              <| fun _ ->
                  let subject = Say.nothing ()
                  Expect.equal subject () "Not an absolute unit"
              testCase "Say hello all"
              <| fun _ ->
                  let subject = Say.hello "all"
                  Expect.equal subject "Hello all" "You didn't say hello" ]

module FirstHwTests =
    [<Tests>]
    let tests =
        testList
            "samples"
            [ testCase "Exponential functions"
              <| fun _ ->

                    // Тесты для (1), наивные степени

                    let input1 = 2
                    let input2 = 1
                    let expected_result = 2
                    let actualResult = FirstHw.silly_pow input1 input2
                    Expect.equal actualResult expected_result (sprintf "%A to the %A power should equal %A" input1 input2 expected_result)

                    let input1 = 14
                    let input2 = -3
                    let expected_result = 0.00036443148688046647
                    let actualResult = FirstHw.silly_pow input1 input2
                    Expect.equal actualResult expected_result (sprintf "%A to the %A power should equal %A" input1 input2 expected_result)

                    let input1 = 3
                    let input2 = -5
                    let expected_result = 0.00411522633744856
                    let actualResult = FirstHw.silly_pow input1 input2
                    Expect.equal actualResult expected_result (sprintf "%A to the %A power should equal %A" input1 input2 expected_result)

                    let input1 = 674
                    let input2 = 3
                    let expected_result = 306182024
                    let actualResult = FirstHw.silly_pow input1 input2
                    Expect.equal actualResult expected_result (sprintf "%A to the %A power should equal %A" input1 input2 expected_result)

                    let input1 = 3242
                    let input2 = 0
                    let expected_result = 1
                    let actualResult = FirstHw.silly_pow input1 input2
                    Expect.equal actualResult expected_result (sprintf "%A to the %A power should equal %A" input1 input2 expected_result)

                    // Тесты для рекурсии silly_pow_rec

                    let input1 = 3
                    let input2 = 8
                    let expected_result = 6561
                    let actualResult = FirstHw.silly_pow_rec input1 input2
                    Expect.equal actualResult expected_result (sprintf "%A to the %A power should equal %A" input1 input2 expected_result)

                    let input1 = 44
                    let input2 = 1
                    let expected_result = 44
                    let actualResult = FirstHw.silly_pow_rec input1 input2
                    Expect.equal actualResult expected_result (sprintf "%A to the %A power should equal %A" input1 input2 expected_result)

                    let input1 = 34534
                    let input2 = 0
                    let expected_result = 1
                    let actualResult = FirstHw.silly_pow_rec input1 input2
                    Expect.equal actualResult expected_result (sprintf "%A to the %A power should equal %A" input1 input2 expected_result)

                    let input1 = 89
                    let input2 = 4
                    let expected_result = 62742241
                    let actualResult = FirstHw.silly_pow_rec input1 input2
                    Expect.equal actualResult expected_result (sprintf "%A to the %A power should equal %A" input1 input2 expected_result)

                    // Тесты для (2), быстрого возведения в степень

                    let input1 = 2
                    let input2 = 1
                    let expected_result = 2
                    let actualResult = FirstHw.fast_pow input1 input2
                    Expect.equal actualResult expected_result (sprintf "%A to the %A power should equal %A" input1 input2 expected_result)

                    let input1 = 0.1
                    let input2 = 0.3
                    let expected_result = 0.5011872336272724
                    let actualResult = FirstHw.fast_pow input1 input2
                    Expect.equal actualResult expected_result (sprintf "%A to the %A power should equal %A" input1 input2 expected_result)

                    let input1 = 1.43
                    let input2 = 5
                    let expected_result = 5.979710894299998
                    let actualResult = FirstHw.fast_pow input1 input2
                    Expect.equal actualResult expected_result (sprintf "%A to the %A power should equal %A" input1 input2 expected_result)

                    let input1 = 7
                    let input2 = 0.6
                    let expected_result = 3.2140958497160383
                    let actualResult = FirstHw.fast_pow input1 input2
                    Expect.equal actualResult expected_result (sprintf "%A to the %A power should equal %A" input1 input2 expected_result)

                    let input1 = 7
                    let input2 = 4
                    let expected_result = 2401
                    let actualResult = FirstHw.fast_pow input1 input2
                    Expect.equal actualResult expected_result (sprintf "%A to the %A power should equal %A" input1 input2 expected_result)


              testCase "Delta between min and max"
              <| fun _ ->

                    // Тесты для (3) функции, находящей разницу между минимальным и максимальным элементом массива

                    let input = [|1; 2; 3; 4; 5|]
                    let expected_result = 4
                    let actualResult = FirstHw.delta input
                    Expect.equal actualResult expected_result (sprintf "Wrong result for %A" input)

                    let input = [|342; 435; 675; 234; 67; 432; 5444|]
                    let expected_result = 5377
                    let actualResult = FirstHw.delta input
                    Expect.equal actualResult expected_result (sprintf "Wrong result for %A" input)

                    let input = [|243|]
                    let expected_result = 0
                    let actualResult = FirstHw.delta input
                    Expect.equal actualResult expected_result (sprintf "Wrong result for %A" input)

                    let input = [|-32; 43; 23; 0|]
                    let expected_result = 75
                    let actualResult = FirstHw.delta input
                    Expect.equal actualResult expected_result (sprintf "Wrong result for %A" input)

                    let input = [|0; 0; 0|]
                    let expected_result = 0
                    let actualResult = FirstHw.delta input
                    Expect.equal actualResult expected_result (sprintf "Wrong result for %A" input)

              testCase "Odd numbers between"
              <| fun _ ->

                    // Тесты для (4), функции генерерирующей массив нечётных чисел,
                    // находящихся строго между входными значениями

                    let input1 = 1
                    let input2 = 1
                    let expected_result = [||]
                    let actualResult = FirstHw.odd_numbers_between input1 input2
                    Expect.equal actualResult expected_result (sprintf "Wrong result for %A and %A" input1 input2)

                    let input1 = 1
                    let input2 = 4
                    let expected_result = [|3|]
                    let actualResult = FirstHw.odd_numbers_between input1 input2
                    Expect.equal actualResult expected_result (sprintf "Wrong result for %A and %A" input1 input2)

                    let input1 = 3
                    let input2 = 22
                    let expected_result = [|5; 7; 9; 11; 13; 15; 17; 19; 21|]
                    let actualResult = FirstHw.odd_numbers_between input1 input2
                    Expect.equal actualResult expected_result (sprintf "Wrong result for %A and %A" input1 input2)

                    let input1 = -54
                    let input2 = -33
                    let expected_result = [|-53; -51; -49; -47; -45; -43; -41; -39; -37; -35|]
                    let actualResult = FirstHw.odd_numbers_between input1 input2
                    Expect.equal actualResult expected_result (sprintf "Wrong result for %A and %A" input1 input2)

            ]
