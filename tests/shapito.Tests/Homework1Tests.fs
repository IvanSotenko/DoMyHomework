module shapito.Tests.Homework1Tests

open Expecto
open shapito

open Homework1

module FirstHwTests =
    [<Tests>]
    let tests =
        testList
            "samples"
            [ testProperty "Testing regular exponential function (silly_pow)"
              <| fun (bas: float) (pow: int) ->
                  let expectedResult = bas ** pow
                  let actualResult = silly_pow bas pow

                  Expect.equal actualResult expectedResult "Не работаит"
              // <| fun _ ->
              //
              //     let input1 = 2.
              //     let input2 = 1
              //     let expected_result = 2
              //
              //     let actualResult = FirstHw.silly_pow input1 input2
              //
              //     Expect.equal
              //         actualResult
              //         expected_result
              //         (sprintf "%A to the %A power should equal %A" input1 input2 expected_result)
              //
              //     let input1 = 14.
              //     let input2 = -3
              //     let expected_result = 0.00036443148688046647
              //
              //     let actualResult = FirstHw.silly_pow input1 input2
              //
              //     Expect.equal
              //         actualResult
              //         expected_result
              //         (sprintf "%A to the %A power should equal %A" input1 input2 expected_result)
              //
              //     let input1 = 3.
              //     let input2 = -5
              //     let expected_result = 0.00411522633744856
              //
              //     let actualResult = FirstHw.silly_pow input1 input2
              //
              //     Expect.equal
              //         actualResult
              //         expected_result
              //         (sprintf "%A to the %A power should equal %A" input1 input2 expected_result)
              //
              //     let input1 = 674
              //     let input2 = 3
              //     let expected_result = 306182024.
              //
              //     let actualResult = FirstHw.silly_pow input1 input2
              //
              //     Expect.equal
              //         actualResult
              //         expected_result
              //         (sprintf "%A to the %A power should equal %A" input1 input2 expected_result)
              //
              //     let input1 = 3242.
              //     let input2 = 0
              //     let expected_result = 1.
              //
              //     let actualResult = FirstHw.silly_pow input1 input2
              //
              //     Expect.equal
              //         actualResult
              //         expected_result
              //         (sprintf "%A to the %A power should equal %A" input1 input2 expected_result)
              //
              //     let input1 = 0.22
              //     let input2 = 4
              //     let expected_result = 0.00234256
              //
              //     let actualResult = FirstHw.silly_pow input1 input2
              //
              //     Expect.equal
              //         actualResult
              //         expected_result
              //         (sprintf "%A to the %A power should equal %A" input1 input2 expected_result)
              //
              //     let input1 = 0.46
              //     let input2 = -3
              //     let expected_result = 0.46 ** -3
              //
              //     let actualResult = FirstHw.silly_pow input1 input2
              //
              //     Expect.equal
              //         actualResult
              //         expected_result
              //         (sprintf "%A to the %A power should equal %A" input1 input2 expected_result)

              testCase "Testing recursive exponential function (silly_pow_rec)"
              <| fun _ ->

                  let input1 = 3.
                  let input2 = 8u
                  let expected_result = 6561.

                  let actualResult = FirstHw.silly_pow_rec input1 input2

                  Expect.equal
                      actualResult
                      expected_result
                      (sprintf "%A to the %A power should equal %A" input1 input2 expected_result)

                  let input1 = 44.
                  let input2 = 1u
                  let expected_result = 44.

                  let actualResult = FirstHw.silly_pow_rec input1 input2

                  Expect.equal
                      actualResult
                      expected_result
                      (sprintf "%A to the %A power should equal %A" input1 input2 expected_result)

                  let input1 = 34534.
                  let input2 = 0u
                  let expected_result = 1.

                  let actualResult = FirstHw.silly_pow_rec input1 input2

                  Expect.equal
                      actualResult
                      expected_result
                      (sprintf "%A to the %A power should equal %A" input1 input2 expected_result)

                  let input1 = 89.
                  let input2 = 4u
                  let expected_result = 62742241.

                  let actualResult = FirstHw.silly_pow_rec input1 input2

                  Expect.equal
                      actualResult
                      expected_result
                      (sprintf "%A to the %A power should equal %A" input1 input2 expected_result)

                  let input1 = 54.84
                  let input2 = 4u
                  let expected_result = 9044608.739535362

                  let actualResult = FirstHw.silly_pow_rec input1 input2

                  Expect.equal
                      actualResult
                      expected_result
                      (sprintf "%A to the %A power should equal %A" input1 input2 expected_result)

                  let input1 = 0.39
                  let input2 = 5u
                  let expected_result = 0.0090224199

                  let actualResult = FirstHw.silly_pow_rec input1 input2

                  Expect.equal
                      actualResult
                      expected_result
                      (sprintf "%A to the %A power should equal %A" input1 input2 expected_result)

                  // Tests for (2) fast_power

                  let input1 = 3.
                  let input2 = 8u
                  let expected_result = 6561.

                  let actualResult = FirstHw.fast_pow input1 input2

                  Expect.equal
                      actualResult
                      expected_result
                      (sprintf "%A to the %A power should equal %A" input1 input2 expected_result)

                  let input1 = 44.
                  let input2 = 1u
                  let expected_result = 44.

                  let actualResult = FirstHw.fast_pow input1 input2

                  Expect.equal
                      actualResult
                      expected_result
                      (sprintf "%A to the %A power should equal %A" input1 input2 expected_result)

                  let input1 = 34534.
                  let input2 = 0u
                  let expected_result = 1.

                  let actualResult = FirstHw.fast_pow input1 input2

                  Expect.equal
                      actualResult
                      expected_result
                      (sprintf "%A to the %A power should equal %A" input1 input2 expected_result)

                  let input1 = 89.
                  let input2 = 4u
                  let expected_result = 62742241.

                  let actualResult = FirstHw.fast_pow input1 input2

                  Expect.equal
                      actualResult
                      expected_result
                      (sprintf "%A to the %A power should equal %A" input1 input2 expected_result)

                  let input1 = 5.83
                  let input2 = 4u
                  let expected_result = 5.83 ** 4

                  let actualResult = FirstHw.fast_pow input1 input2

                  Expect.equal
                      actualResult
                      expected_result
                      (sprintf "%A to the %A power should equal %A" input1 input2 expected_result)

                  let input1 = 0.39
                  let input2 = 3u
                  let expected_result = 0.39 ** 3

                  let actualResult = FirstHw.fast_pow input1 input2

                  Expect.equal
                      actualResult
                      expected_result
                      (sprintf "%A to the %A power should equal %A" input1 input2 expected_result)


              testCase "Delta between min and max"
              <| fun _ ->

                  // Tests for function for the difference between the largest and smallest element of the array

                  let input = [| 1; 2; 3; 4; 5 |]
                  let expected_result = 4
                  let actualResult = FirstHw.delta input
                  Expect.equal actualResult expected_result (sprintf "Wrong result for %A" input)

                  let input = [| 342; 435; 675; 234; 67; 432; 5444 |]

                  let expected_result = 5377
                  let actualResult = FirstHw.delta input
                  Expect.equal actualResult expected_result (sprintf "Wrong result for %A" input)

                  let input = [| 243 |]
                  let expected_result = 0
                  let actualResult = FirstHw.delta input
                  Expect.equal actualResult expected_result (sprintf "Wrong result for %A" input)

                  let input = [| -32; 43; 23; 0 |]
                  let expected_result = 75
                  let actualResult = FirstHw.delta input
                  Expect.equal actualResult expected_result (sprintf "Wrong result for %A" input)

                  let input = [| 0; 0; 0 |]
                  let expected_result = 0
                  let actualResult = FirstHw.delta input
                  Expect.equal actualResult expected_result (sprintf "Wrong result for %A" input)

                  let input = [||]
                  let expected_error = "Error in Main.fs/FirstHw/delta: empty array was given. Doesn't make sense"
                  Expect.throws (fun _ -> FirstHw.delta input |> ignore) expected_error

              testCase "Odd numbers between"
              <| fun _ ->

                  // Tests for (4), function, that generate array of odd numbers
                  // between the two input values

                  let input1 = 1
                  let input2 = 1
                  let expected_result = [||]

                  let actualResult = FirstHw.odd_numbers_between input1 input2

                  Expect.equal actualResult expected_result (sprintf "Wrong result for %A and %A" input1 input2)

                  let input1 = 1
                  let input2 = 4
                  let expected_result = [| 3 |]

                  let actualResult = FirstHw.odd_numbers_between input1 input2

                  Expect.equal actualResult expected_result (sprintf "Wrong result for %A and %A" input1 input2)

                  let input1 = 3
                  let input2 = 22

                  let expected_result = [| 5; 7; 9; 11; 13; 15; 17; 19; 21 |]

                  let actualResult = FirstHw.odd_numbers_between input1 input2

                  Expect.equal actualResult expected_result (sprintf "Wrong result for %A and %A" input1 input2)

                  let input1 = -54
                  let input2 = -33

                  let expected_result =
                      [| -53
                         -51
                         -49
                         -47
                         -45
                         -43
                         -41
                         -39
                         -37
                         -35 |]

                  let actualResult = FirstHw.odd_numbers_between input1 input2

                  Expect.equal actualResult expected_result (sprintf "Wrong result for %A and %A" input1 input2)

                  let input1 = 5
                  let input2 = 4

                  let expected_result = [||]

                  let actualResult = FirstHw.odd_numbers_between input1 input2

                  Expect.equal actualResult expected_result (sprintf "Wrong result for %A and %A" input1 input2)

              ]
