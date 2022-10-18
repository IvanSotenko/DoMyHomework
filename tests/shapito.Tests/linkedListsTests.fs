module DoMyHomework.Tests.linkedListsTests

open DoMyHomework
open Expecto
open FsCheck

open Convertor

let config = { Config.Default with MaxTest = 10000 }

[<Tests>]
let MyListTests =

    testList
        "Test for MyList"
        [

          // Conversions
          testProperty "List -> MyList -> List is the original list"
          <| fun list ->
              let result = MyListToList(ListToMyList list)
              Expect.equal list result "The results were different"

          testProperty "(List ->) OOPList -> MyList -> OOPList (-> List) is the original List"
          <| fun list ->
              let result =
                  list
                  |> ListToOOPList
                  |> OOPListToMyList
                  |> MyListToOOPList
                  |> OOPListToList

              Expect.equal list result "The results were different"


          // Concatenation
          testProperty "(MyList.concat list1 list2) is (list1 @ list2)"
          <| fun list1 list2 ->
              let actualResult =
                  MyListToList(MyList.concat (list1 |> ListToMyList) (list2 |> ListToMyList))

              let expectedResult = list1 @ list2
              Expect.equal actualResult expectedResult "The results were different"

          testProperty "(MyList.concat list []) is (MyList.concat [] list)"
          <| fun list ->
              let result1 =
                  (MyList.concat (list |> ListToMyList) ([] |> ListToMyList))
                  |> MyListToList

              let result2 =
                  (MyList.concat ([] |> ListToMyList) (list |> ListToMyList))
                  |> MyListToList

              Expect.equal result1 result2 "The results were different"

          testCase "MyList.concat [] [] is []"
          <| fun _ ->
              let result =
                  (MyList.concat (ListToMyList []) (ListToMyList []))
                  |> MyListToList

              Expect.equal result [] "The results were different"


          // BubbleSort
          testProperty "(List ->) MyList.bubbleSort (-> List) should give the same result as List.Sort (int)"
          <| fun (list: list<int>) ->
              let actualResult =
                  MyListToList
                  <| (MyList.bubbleSort (ListToMyList <| list))

              let expectedResult = List.sort list
              Expect.equal actualResult expectedResult "The results were different"

          testProperty "(List ->) MyList.bubbleSort (-> List) should give the same result as List.Sort (float)"
          <| fun (list: list<NormalFloat>) ->
              let actualResult =
                  MyListToList
                  <| (MyList.bubbleSort (ListToMyList <| list))

              let expectedResult = List.sort list
              Expect.equal actualResult expectedResult "The results were different"

          testProperty "(List ->) MyList.bubbleSort (-> List) should give the same result as List.Sort (string)"
          <| fun (list: list<string>) ->
              let actualResult =
                  MyListToList
                  <| (MyList.bubbleSort (ListToMyList <| list))

              let expectedResult = List.sort list
              Expect.equal actualResult expectedResult "The results were different"

          testProperty "(List ->) MyList.bubbleSort (-> List) should give the same result as List.Sort (char)"
          <| fun (list: list<char>) ->
              let actualResult =
                  MyListToList
                  <| (MyList.bubbleSort (ListToMyList <| list))

              let expectedResult = List.sort list
              Expect.equal actualResult expectedResult "The results were different"

          testCase "[] -> MyList -> MyList.bubbleSort -> List is []"
          <| fun _ ->
              let result =
                  []
                  |> ListToMyList
                  |> MyList.bubbleSort
                  |> MyListToList

              Expect.equal result [] "The results were different"


          // QuickSort
          testProperty "(List ->) MyList.quickSort (-> List) should give the same result as List.Sort (int)"
          <| fun (list: list<int>) ->
              let actualResult =
                  MyListToList
                  <| (MyList.quickSort (ListToMyList <| list))

              let expectedResult = List.sort list
              Expect.equal actualResult expectedResult "The results were different"

          testProperty "(List ->) MyList.quickSort (-> List) should give the same result as List.Sort (float)"
          <| fun (list: list<NormalFloat>) ->
              let actualResult =
                  MyListToList
                  <| (MyList.quickSort (ListToMyList <| list))

              let expectedResult = List.sort list
              Expect.equal actualResult expectedResult "The results were different"

          testProperty "(List ->) MyList.quickSort (-> List) should give the same result as List.Sort (string)"
          <| fun (list: list<string>) ->
              let actualResult =
                  MyListToList
                  <| (MyList.quickSort (ListToMyList <| list))

              let expectedResult = List.sort list
              Expect.equal actualResult expectedResult "The results were different"

          testProperty "(List ->) MyList.quickSort (-> List) should give the same result as List.Sort (char)"
          <| fun (list: list<char>) ->
              let actualResult =
                  MyListToList
                  <| (MyList.quickSort (ListToMyList <| list))

              let expectedResult = List.sort list
              Expect.equal actualResult expectedResult "The results were different"

          testCase "[] -> MyList -> MyList.quickSort -> List is []"
          <| fun _ ->
              let result =
                  []
                  |> ListToMyList
                  |> MyList.quickSort
                  |> MyListToList

              Expect.equal result [] "The results were different" ]



[<Tests>]
let OOPListTests =

    testList
        "Test for OOPList"
        [

          // Conversions
          testProperty "List -> OOPList -> List is the original list"
          <| fun list ->
              let result = OOPListToList(ListToOOPList list)
              Expect.equal list result "The results were different"

          testProperty "(List ->) MyList -> OOPList -> MyList (-> List) is the original List"
          <| fun list ->
              let result =
                  list
                  |> ListToMyList
                  |> MyListToOOPList
                  |> OOPListToMyList
                  |> MyListToList

              Expect.equal list result "The results were different"


          // Concatenation
          testProperty "(OOPList.concat list1 list2) is (list1 @ list2)"
          <| fun list1 list2 ->
              let actualResult =
                  OOPListToList(OOPList.concat (list1 |> ListToOOPList) (list2 |> ListToOOPList))

              let expectedResult = list1 @ list2
              Expect.equal actualResult expectedResult "The results were different"

          testProperty "(OOPList.concat list []) is (OOPList.concat [] list)"
          <| fun list ->
              let result1 =
                  (OOPList.concat (list |> ListToOOPList) ([] |> ListToOOPList))
                  |> OOPListToList

              let result2 =
                  (OOPList.concat ([] |> ListToOOPList) (list |> ListToOOPList))
                  |> OOPListToList

              Expect.equal result1 result2 "The results were different"

          testCase "OOPList.concat [] [] is []"
          <| fun _ ->
              let result =
                  (OOPList.concat (ListToOOPList []) (ListToOOPList []))
                  |> OOPListToList

              Expect.equal result [] "The results were different"


          // BubbleSort
          testProperty "(List ->) OOPList.bubbleSort (-> List) should give the same result as List.Sort (int)"
          <| fun (list: list<int>) ->
              let actualResult =
                  OOPListToList
                  <| (OOPList.bubbleSort (ListToOOPList <| list))

              let expectedResult = List.sort list
              Expect.equal actualResult expectedResult "The results were different"

          testProperty "(List ->) OOPList.bubbleSort (-> List) should give the same result as List.Sort (float)"
          <| fun (list: list<NormalFloat>) ->
              let actualResult =
                  OOPListToList
                  <| (OOPList.bubbleSort (ListToOOPList <| list))

              let expectedResult = List.sort list
              Expect.equal actualResult expectedResult "The results were different"

          testProperty "(List ->) OOPList.bubbleSort (-> List) should give the same result as List.Sort (strings)"
          <| fun (list: list<string>) ->
              let actualResult =
                  OOPListToList
                  <| (OOPList.bubbleSort (ListToOOPList <| list))

              let expectedResult = List.sort list
              Expect.equal actualResult expectedResult "The results were different"

          testProperty "(List ->) OOPList.bubbleSort (-> List) should give the same result as List.Sort (char)"
          <| fun (list: list<char>) ->
              let actualResult =
                  OOPListToList
                  <| (OOPList.bubbleSort (ListToOOPList <| list))

              let expectedResult = List.sort list
              Expect.equal actualResult expectedResult "The results were different"

          testCase "[] -> OOPList -> OOPList.bubbleSort -> List is []"
          <| fun _ ->
              let result =
                  []
                  |> ListToOOPList
                  |> OOPList.bubbleSort
                  |> OOPListToList

              Expect.equal result [] "The results were different"


          // QuickSort
          testProperty "(List ->) OOPList.quickSort (-> List) should give the same result as List.Sort (int)"
          <| fun (list: list<int>) ->
              let actualResult =
                  OOPListToList
                  <| (OOPList.quickSort (ListToOOPList <| list))

              let expectedResult = List.sort list
              Expect.equal actualResult expectedResult "The results were different"

          testProperty "(List ->) OOPList.quickSort (-> List) should give the same result as List.Sort (float)"
          <| fun (list: list<NormalFloat>) ->
              let actualResult =
                  OOPListToList
                  <| (OOPList.quickSort (ListToOOPList <| list))

              let expectedResult = List.sort list
              Expect.equal actualResult expectedResult "The results were different"

          testProperty "(List ->) OOPList.quickSort (-> List) should give the same result as List.Sort (string)"
          <| fun (list: list<string>) ->
              let actualResult =
                  OOPListToList
                  <| (OOPList.quickSort (ListToOOPList <| list))

              let expectedResult = List.sort list
              Expect.equal actualResult expectedResult "The results were different"

          testProperty "(List ->) OOPList.quickSort (-> List) should give the same result as List.Sort (char)"
          <| fun (list: list<char>) ->
              let actualResult =
                  OOPListToList
                  <| (OOPList.quickSort (ListToOOPList <| list))

              let expectedResult = List.sort list
              Expect.equal actualResult expectedResult "The results were different"

          testCase "[] -> OOPList -> OOPList.quickSort -> List is []"
          <| fun _ ->
              let result =
                  []
                  |> ListToOOPList
                  |> OOPList.quickSort
                  |> OOPListToList

              Expect.equal result [] "The results were different"

          ]
