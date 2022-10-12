module shapito.Tests.TestExamples

open Expecto

module SayTests =
    [<Tests>]
    let tests =
        testList
            "samples"
            [ testCase "Say nothing"
              <| fun _ ->
                  let subject = Say.Say.nothing ()
                  Expect.equal subject () "Not an absolute unit"
              testCase "Say hello all"
              <| fun _ ->
                  let subject = Say.Say.hello "all"
                  Expect.equal subject "Hello all" "You didn't say hello" ]
