module shapito.Tests.linkedListsTests

open shapito
open Expecto
open FsCheck

open Translator

let config = { Config.Default with MaxTest = 10000 }

[<Tests>]
let tests =

    testList "samples" [

        testProperty "List -> MyList -> List should return the original list"
            <| fun lst ->
                let result = MyListToList (listToMyList lst)
                Expect.equal lst result "The results were different"

        testCase "BubbleSort (MyList): Edge case of a single element"
            <| fun _ ->
                let a = [4; 1; 6; 4; 8]
                let expectedResult = List.sort a
                let actualResult = MyListToList <| (MyList.bubbleSort <| (listToMyList <| a))
                Expect.equal expectedResult actualResult "The results were different"

]
