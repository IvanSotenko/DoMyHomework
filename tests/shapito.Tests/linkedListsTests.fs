module shapito.Tests.linkedListsTests

open shapito
open Expecto
open FsCheck

open Translator

let config = { Config.Default with MaxTest = 10000 }

[<Tests>]
let MyListTests =

    testList "Test for MyList" [

        // Conversions
        testProperty "List -> MyList -> List is the original list"
            <| fun lst ->
                let result = MyListToList (listToMyList lst)
                Expect.equal lst result "The results were different"

        testProperty "(List ->) OOPList -> MyList -> OOPList (-> List) is the original List"
            <| fun lst ->
                let result = lst |> listToOOPList |> OOPListToMyList |> MyListToOOPList |> OOPListToList
                Expect.equal lst result "The results were different"


        // Concatenation
        testProperty "(MyList.concat list1 list2) is (list1 @ list2)"
            <| fun lst1 lst2 ->
               let actualResult = MyListToList (MyList.concat (lst1 |> listToMyList) (lst2 |> listToMyList))
               let expectedResult = lst1 @ lst2
               Expect.equal actualResult expectedResult "The results were different"

        testProperty "(MyList.concat list []) is (MyList.concat [] list)"
            <| fun lst ->
                let result1 = (MyList.concat (lst |> listToMyList) ([] |> listToMyList)) |> MyListToList
                let result2 = (MyList.concat ([] |> listToMyList) (lst |> listToMyList)) |> MyListToList
                Expect.equal result1 result2 "The results were different"

        testCase "MyList.concat [] [] is []"
            <| fun _ ->
                let result = (MyList.concat (listToMyList []) (listToMyList [])) |> MyListToList
                Expect.equal result [] "The results were different"


        // BubbleSort
        testProperty "(List ->) MyList.bubbleSort (-> List) should give the same result as List.Sort (nums)"
            <| fun (lst: list<int>) ->
                let actualResult = MyListToList <| (MyList.bubbleSort (listToMyList <| lst))
                let expectedResult = List.sort lst
                Expect.equal actualResult expectedResult "The results were different"

        testProperty "(List ->) MyList.bubbleSort (-> List) should give the same result as List.Sort (strings)"
            <| fun (lst: list<string>) ->
                let actualResult = MyListToList <| (MyList.bubbleSort (listToMyList <| lst))
                let expectedResult = List.sort lst
                Expect.equal actualResult expectedResult "The results were different"

        testCase "[] -> MyList -> MyList.bubbleSort -> List is []"
            <| fun _ ->
                let result = [] |> listToMyList |> MyList.bubbleSort |> MyListToList
                Expect.equal result [] "The results were different"


        // QuickSort
        testProperty "(List ->) MyList.quickSort (-> List) should give the same result as List.Sort (nums)"
            <| fun (lst: list<int>) ->
                let actualResult = MyListToList <| (MyList.quickSort (listToMyList <| lst))
                let expectedResult = List.sort lst
                Expect.equal actualResult expectedResult "The results were different"

        testProperty "(List ->) MyList.quickSort (-> List) should give the same result as List.Sort (strings)"
            <| fun (lst: list<string>) ->
                let actualResult = MyListToList <| (MyList.quickSort (listToMyList <| lst))
                let expectedResult = List.sort lst
                Expect.equal actualResult expectedResult "The results were different"

        testCase "[] -> MyList -> MyList.quickSort -> List is []"
            <| fun _ ->
                let result = [] |> listToMyList |> MyList.quickSort |> MyListToList
                Expect.equal result [] "The results were different"
    ]

[<Tests>]
let OOPListTests =

    testList "Test for OOPList" [

        // Conversions
        testProperty "List -> OOPList -> List is the original list"
            <| fun lst ->
                let result = OOPListToList (listToOOPList lst)
                Expect.equal lst result "The results were different"

        testProperty "(List ->) MyList -> OOPList -> MyList (-> List) is the original List"
            <| fun lst ->
                let result = lst |> listToMyList |> MyListToOOPList |> OOPListToMyList |> MyListToList
                Expect.equal lst result "The results were different"


        // Concatenation
        testProperty "(OOPList.concat list1 list2) is (list1 @ list2)"
            <| fun lst1 lst2 ->
               let actualResult = OOPListToList (OOPList.concat (lst1 |> listToOOPList) (lst2 |> listToOOPList))
               let expectedResult = lst1 @ lst2
               Expect.equal actualResult expectedResult "The results were different"

        testProperty "(OOPList.concat list []) is (OOPList.concat [] list)"
            <| fun lst ->
                let result1 = (OOPList.concat (lst |> listToOOPList) ([] |> listToOOPList)) |> OOPListToList
                let result2 = (OOPList.concat ([] |> listToOOPList) (lst |> listToOOPList)) |> OOPListToList
                Expect.equal result1 result2 "The results were different"

        testCase "OOPList.concat [] [] is []"
            <| fun _ ->
                let result = (OOPList.concat (listToOOPList []) (listToOOPList [])) |> OOPListToList
                Expect.equal result [] "The results were different"


        // BubbleSort
        testProperty "(List ->) OOPList.bubbleSort (-> List) should give the same result as List.Sort (nums)"
            <| fun (lst: list<int>) ->
                let actualResult = OOPListToList <| (OOPList.bubbleSort (listToOOPList <| lst))
                let expectedResult = List.sort lst
                Expect.equal actualResult expectedResult "The results were different"

        testProperty "(List ->) OOPList.bubbleSort (-> List) should give the same result as List.Sort (strings)"
            <| fun (lst: list<string>) ->
                let actualResult = OOPListToList <| (OOPList.bubbleSort (listToOOPList <| lst))
                let expectedResult = List.sort lst
                Expect.equal actualResult expectedResult "The results were different"

        testCase "[] -> OOPList -> OOPList.bubbleSort -> List is []"
            <| fun _ ->
                let result = [] |> listToOOPList |> OOPList.bubbleSort |> OOPListToList
                Expect.equal result [] "The results were different"


        // QuickSort
        testProperty "(List ->) OOPList.quickSort (-> List) should give the same result as List.Sort (nums)"
            <| fun (lst: list<int>) ->
                let actualResult = OOPListToList <| (OOPList.quickSort (listToOOPList <| lst))
                let expectedResult = List.sort lst
                Expect.equal actualResult expectedResult "The results were different"

        testProperty "(List ->) OOPList.quickSort (-> List) should give the same result as List.Sort (strings)"
            <| fun (lst: list<string>) ->
                let actualResult = OOPListToList <| (OOPList.quickSort (listToOOPList <| lst))
                let expectedResult = List.sort lst
                Expect.equal actualResult expectedResult "The results were different"

        testCase "[] -> OOPList -> OOPList.quickSort -> List is []"
            <| fun _ ->
                let result = [] |> listToOOPList |> OOPList.quickSort |> OOPListToList
                Expect.equal result [] "The results were different"

    ]
