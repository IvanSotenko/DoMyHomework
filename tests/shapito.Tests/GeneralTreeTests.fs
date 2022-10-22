module DoMyHomework.Tests.GeneralTreeTests

(*
1. Как я говорил, тесты свойств не отменяют обычные "ручные" тесты.
2. Свойства - это не только про эквивалентность поведения двух функций.
Это про любые адекватные свойства функций.
Например, количество различных элементов вряд ли может быть больше, чем всего элементов собранных в список.
*)
open DoMyHomework
open Expecto
open FsCheck

open GeneralTree
open MyList

let config = { Config.Default with MaxTest = 10000 }

[<Tests>]
let MyListTests =

    testList
        "Test for GeneralTree"
        [
            // countDistinct
            testCase "One element tree"
            <| fun _ ->
                let expectedResult = 1

                let actualResult = countDistinct (Node (4, Empty))
                Expect.equal actualResult expectedResult "The results were different"

                let actualResult = countDistinct (Node (4.12, Empty))
                Expect.equal actualResult expectedResult "The results were different"

                let actualResult = countDistinct (Node ("finn", Empty))
                Expect.equal actualResult expectedResult "The results were different"

                let actualResult = countDistinct (Node ('a', Empty))
                Expect.equal actualResult expectedResult "The results were different"

            testCase "Common input data #1"
            <| fun _ ->
                let input = Node (1, Cons (Node (2, Cons (Node (1, Empty), Cons (Node (3, Empty), Cons (Node (0, Empty), Cons (Node (-4, Empty), Empty))))), Cons (Node (3, Empty), Empty)))
                let actualResult = countDistinct input
                let expectedResult = 5
                Expect.equal actualResult expectedResult "The results were different"

            testCase "Common input data #2"
            <| fun _ ->
                let input = Node ("jake", Cons (Node ("bubblegum", Empty), Cons (Node ("marceline", Cons (Node ("bubblegum", Empty), Empty)), Empty)))
                let actualResult = countDistinct input
                let expectedResult = 3
                Expect.equal actualResult expectedResult "The results were different"

            testCase "Common input data #3"
            <| fun _ ->
                let input = Node (3.2345, Cons (Node (9.2345, Cons (Node (2.4232, Empty), Cons (Node (3.2345, Empty), Empty))), Cons (Node (2.4232, Cons (Node (9.2333, Empty), Empty)), Empty)))
                let actualResult = countDistinct input
                let expectedResult = 4
                Expect.equal actualResult expectedResult "The results were different"

        ]
