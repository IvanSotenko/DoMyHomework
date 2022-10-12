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
            <| fun (lst: list<'value>) ->
                let result = (MyList.concat (lst |> listToMyList) ([] |> listToMyList)) =  (MyList.concat ([] |> listToMyList) (lst |> listToMyList))
                Expect.equal result true "The results were different"

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
