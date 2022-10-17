module shapito.Tests.Homework1Tests

open Expecto
open shapito

open Homework1

[<Tests>]
let tests =
    testList
        "samples"
        [

          // Silly power tests
          testProperty "silly_pow a b should give the same result as (a**b)"
          <| fun (bas: float) (pow: int) ->
              let expectedResult = bas ** pow
              let actualResult = silly_pow bas pow

              if (expectedResult = infinity)
                 || (expectedResult = (-infinity)) then
                  Expect.equal
                      actualResult
                      expectedResult
                      (sprintf "%A^%A: actualResult=%A, expectedResult=%A" bas pow actualResult expectedResult)
              elif System.Double.IsNaN expectedResult then
                  Expect.isTrue
                      (System.Double.IsNaN actualResult)
                      (sprintf "%A^%A: actualResult=%A, expectedResult=%A" bas pow actualResult expectedResult)
              else
                  Expect.floatClose
                      Accuracy.high
                      actualResult
                      expectedResult
                      (sprintf "%A^%A: actualResult=%A, expectedResult=%A" bas pow actualResult expectedResult)


          //Silly recursive power tests
          testProperty "silly_pow_rec a b should give the same result as (a**b))"
          <| fun (bas: float) (pow: uint) ->
              let expectedResult = bas ** (int pow)
              let actualResult = silly_pow_rec bas pow

              if (expectedResult = infinity)
                 || (expectedResult = (-infinity)) then
                  Expect.equal
                      actualResult
                      expectedResult
                      (sprintf "%A^%A: actualResult=%A, expectedResult=%A" bas pow actualResult expectedResult)
              elif System.Double.IsNaN expectedResult then
                  Expect.isTrue
                      (System.Double.IsNaN actualResult)
                      (sprintf "%A^%A: actualResult=%A, expectedResult=%A" bas pow actualResult expectedResult)
              else
                  Expect.floatClose
                      Accuracy.high
                      actualResult
                      expectedResult
                      (sprintf "%A^%A: actualResult=%A, expectedResult=%A" bas pow actualResult expectedResult)


          //Fast power tests
          testProperty "fast_pow a b should give the same result as (a**b)"
          <| fun (bas: float) (pow: uint) ->
              let expectedResult = bas ** (int pow)
              let actualResult = fast_pow bas pow

              if (expectedResult = infinity)
                 || (expectedResult = (-infinity)) then
                  Expect.equal
                      actualResult
                      expectedResult
                      (sprintf "%A^%A: actualResult=%A, expectedResult=%A" bas pow actualResult expectedResult)
              elif System.Double.IsNaN expectedResult then
                  Expect.isTrue
                      (System.Double.IsNaN actualResult)
                      (sprintf "%A^%A: actualResult=%A, expectedResult=%A" bas pow actualResult expectedResult)
              else
                  Expect.floatClose
                      Accuracy.high
                      actualResult
                      expectedResult
                      (sprintf "%A^%A: actualResult=%A, expectedResult=%A" bas pow actualResult expectedResult)


          // Difference between max and min element of array
          testProperty "(delta arr) should work the same as ((Array.max arr) - (Array.min arr))"
          <| fun arr ->
              if arr = [||] then
                  let expectedError =
                      "Error in Main.fs/FirstHw/delta: empty array was given. Doesn't make sense"

                  Expect.throws (fun _ -> FirstHw.delta arr |> ignore) expectedError
              else
                  let expectedResult = (Array.max arr) - (Array.min arr)
                  let actualResult = delta arr
                  Expect.equal actualResult expectedResult ""


          // Tests for odd_numbers_between, function, that generate array of odd numbers
          // between the two input values
          testCase "If a = b, then odd_numbers_between a b is [||]"
          <| fun _ ->
              let input1 = 1
              let input2 = 1
              let expectedResult = [||]
              let actualResult = FirstHw.odd_numbers_between input1 input2

              Expect.equal
                  actualResult
                  expectedResult
                  (sprintf
                      "odd_numbers_between %A %A, actualResult=%A, expectedResult=%A"
                      input1
                      input2
                      actualResult
                      expectedResult)

          testCase "Common data for odd_numbers_between #1"
          <| fun _ ->
              let input1 = 1
              let input2 = 4
              let expectedResult = [| 3 |]
              let actualResult = FirstHw.odd_numbers_between input1 input2

              Expect.equal
                  actualResult
                  expectedResult
                  (sprintf
                      "odd_numbers_between %A %A, actualResult=%A, expectedResult=%A"
                      input1
                      input2
                      actualResult
                      expectedResult)

          testCase "Common data for odd_numbers_between #2"
          <| fun _ ->
              let input1 = 3
              let input2 = 22
              let expectedResult = [| 5; 7; 9; 11; 13; 15; 17; 19; 21 |]
              let actualResult = FirstHw.odd_numbers_between input1 input2

              Expect.equal
                  actualResult
                  expectedResult
                  (sprintf
                      "odd_numbers_between %A %A, actualResult=%A, expectedResult=%A"
                      input1
                      input2
                      actualResult
                      expectedResult)

          testCase "Common data for odd_numbers_between #3"
          <| fun _ ->
              let input1 = -54
              let input2 = -33

              let expectedResult =
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

              Expect.equal
                  actualResult
                  expectedResult
                  (sprintf
                      "odd_numbers_between %A %A, actualResult=%A, expectedResult=%A"
                      input1
                      input2
                      actualResult
                      expectedResult)

          testCase "Common data for odd_numbers_between #4"
          <| fun _ ->
              let input1 = 5
              let input2 = 4
              let expectedResult = [||]
              let actualResult = FirstHw.odd_numbers_between input1 input2

              Expect.equal
                  actualResult
                  expectedResult
                  (sprintf
                      "odd_numbers_between %A %A, actualResult=%A, expectedResult=%A"
                      input1
                      input2
                      actualResult
                      expectedResult)

          ]
