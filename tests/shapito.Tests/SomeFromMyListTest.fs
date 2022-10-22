module DoMyHomework.Tests.SomeFromMyListTest

open DoMyHomework
open Expecto
open FsCheck

open MyList

let config = { Config.Default with MaxTest = 10000 }

[<Tests>]
let MyListTests =

    testList
        "Test for MyList.toSet and MyList.length"
        [
          // MyList.toSet
          testProperty "(list |> MyList.ofList |> toSet) should be equal to (list |> Set.ofList) (int)"
          <| fun (list: list<int>) ->
              let actualResult = list |> ofList |> toSet
              let expectedResult = list |> Set.ofList
              Expect.equal actualResult expectedResult "The results were different"

          testProperty "(list |> MyList.ofList |> toSet) should be equal to (list |> Set.ofList) (string)"
          <| fun (list: list<string>) ->
              let actualResult = list |> ofList |> toSet
              let expectedResult = list |> Set.ofList
              Expect.equal actualResult expectedResult "The results were different"

          testProperty "(list |> MyList.ofList |> toSet) should be equal to (list |> Set.ofList) (char)"
          <| fun (list: list<char>) ->
              let actualResult = list |> ofList |> toSet
              let expectedResult = list |> Set.ofList
              Expect.equal actualResult expectedResult "The results were different"

          testProperty "(list |> MyList.ofList |> toSet) should be equal to (list |> Set.ofList) (float)"
          <| fun (list: list<NormalFloat>) ->
              let actualResult = list |> ofList |> toSet
              let expectedResult = list |> Set.ofList
              Expect.equal actualResult expectedResult "The results were different"



          // MyList.length
          testProperty "(list |> MyList.ofList |> length) should be equal to (list |> length) (int)"
          <| fun (list: list<int>) ->
              let actualResult = list |> ofList |> length
              let expectedResult = list |> List.length
              Expect.equal actualResult expectedResult "The results were different"

          testProperty "(list |> MyList.ofList |> length) should be equal to (list |> length) (string)"
          <| fun (list: list<string>) ->
              let actualResult = list |> ofList |> length
              let expectedResult = list |> List.length
              Expect.equal actualResult expectedResult "The results were different"

          testProperty "(list |> MyList.ofList |> length) should be equal to (list |> length) (char)"
          <| fun (list: list<char>) ->
              let actualResult = list |> ofList |> length
              let expectedResult = list |> List.length
              Expect.equal actualResult expectedResult "The results were different"

          testProperty "(list |> MyList.ofList |> length) should be equal to (list |> length) (float)"
          <| fun (list: list<NormalFloat>) ->
              let actualResult = list |> ofList |> length
              let expectedResult = list |> List.length
              Expect.equal actualResult expectedResult "The results were different" ]
