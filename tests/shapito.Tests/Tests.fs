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

                    let actualResult = First_hw.raise_to_silly_power 234 0
                    Expect.equal actualResult 1 "234 to the 0 power should equal 1"

                    let actualResult = First_hw.raise_to_silly_power 234 0
                    Expect.equal actualResult 1 "234 to the 0 power should equal 1"

                    let actualResult = First_hw.raise_to_silly_power 234 0
                    Expect.equal actualResult 1 "234 to the 0 power should equal 1"

                    let actualResult = First_hw.raise_to_silly_power 234 0
                    Expect.equal actualResult 1 "234 to the 0 power should equal 1"
            ]
