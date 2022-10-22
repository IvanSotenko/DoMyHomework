module DoMyHomework.Tests.GeneralTreeTests

open DoMyHomework
open Expecto
open FsCheck

open GeneralTree
open MyList

let config = { Config.Default with MaxTest = 10000 }

[<Tests>]
let GeneralTreeTests =

    testList
        "Test for GeneralTree"
        [
          // countDistinct
          testCase "One element tree for countDistinct"
          <| fun _ ->
              let expectedResult = 1

              let actualResult = countDistinct (Node(4, Empty))
              Expect.equal actualResult expectedResult "The results were different"

              let actualResult = countDistinct (Node(4.12, Empty))
              Expect.equal actualResult expectedResult "The results were different"

              let actualResult = countDistinct (Node("finn", Empty))
              Expect.equal actualResult expectedResult "The results were different"

              let actualResult = countDistinct (Node('a', Empty))
              Expect.equal actualResult expectedResult "The results were different"

          testCase "Common input data for countDistinct #1"
          <| fun _ ->
              let input =
                  Node(
                      1,
                      Cons(
                          Node(
                              2,
                              Cons(
                                  Node(1, Empty),
                                  Cons(Node(3, Empty), Cons(Node(0, Empty), Cons(Node(-4, Empty), Empty)))
                              )
                          ),
                          Cons(Node(3, Empty), Empty)
                      )
                  )

              let actualResult = countDistinct input
              let expectedResult = 5
              Expect.equal actualResult expectedResult "The results were different"

          testCase "Common input data for countDistinct #2"
          <| fun _ ->
              let input =
                  Node(
                      "jake",
                      Cons(
                          Node("bubblegum", Empty),
                          Cons(Node("marceline", Cons(Node("bubblegum", Empty), Empty)), Empty)
                      )
                  )

              let actualResult = countDistinct input
              let expectedResult = 3
              Expect.equal actualResult expectedResult "The results were different"

          testCase "Common input data for countDistinct #3"
          <| fun _ ->
              let input =
                  Node(
                      3.2345,
                      Cons(
                          Node(9.2345, Cons(Node(2.4232, Empty), Cons(Node(3.2345, Empty), Empty))),
                          Cons(Node(2.4232, Cons(Node(9.2333, Empty), Empty)), Empty)
                      )
                  )

              let actualResult = countDistinct input
              let expectedResult = 4
              Expect.equal actualResult expectedResult "The results were different"



          // GeneralTree.toMyList
          testCase "One element tree for GeneralTree.toMyList"
          <| fun _ ->
              let actualResult = toMyList (Node(4, Empty))
              let expectedResult = Cons(4, Empty)
              Expect.equal actualResult expectedResult "The results were different"

              let actualResult = toMyList (Node(4.12, Empty))
              let expectedResult = Cons(4.12, Empty)
              Expect.equal actualResult expectedResult "The results were different"

              let actualResult = toMyList (Node("finn", Empty))
              let expectedResult = Cons("finn", Empty)
              Expect.equal actualResult expectedResult "The results were different"

              let actualResult = toMyList (Node('a', Empty))
              let expectedResult = Cons('a', Empty)
              Expect.equal actualResult expectedResult "The results were different"

          testCase "Common input data for GeneralTree.toMyList #1"
          <| fun _ ->
              let input =
                  Node(
                      1,
                      Cons(
                          Node(
                              2,
                              Cons(
                                  Node(1, Empty),
                                  Cons(Node(3, Empty), Cons(Node(0, Empty), Cons(Node(-4, Empty), Empty)))
                              )
                          ),
                          Cons(Node(3, Empty), Empty)
                      )
                  )

              let actualResult = toMyList input

              let expectedResult =
                  Cons(1, Cons(2, Cons(1, Cons(3, Cons(0, Cons(-4, Cons(3, Empty)))))))

              Expect.equal actualResult expectedResult "The results were different"

          testCase "Common input data for GeneralTree.toMyList #2"
          <| fun _ ->
              let input =
                  Node(
                      "jake",
                      Cons(
                          Node("bubblegum", Empty),
                          Cons(Node("marceline", Cons(Node("bubblegum", Empty), Empty)), Empty)
                      )
                  )

              let actualResult = toMyList input

              let expectedResult =
                  Cons("jake", Cons("bubblegum", Cons("marceline", Cons("bubblegum", Empty))))

              Expect.equal actualResult expectedResult "The results were different"

          testCase "Common input data for GeneralTree.toMyList #3"
          <| fun _ ->
              let input =
                  Node(
                      3.2345,
                      Cons(
                          Node(9.2345, Cons(Node(2.4232, Empty), Cons(Node(3.2345, Empty), Empty))),
                          Cons(Node(2.4232, Cons(Node(9.2333, Empty), Empty)), Empty)
                      )
                  )

              let actualResult = toMyList input

              let expectedResult =
                  Cons(3.2345, Cons(9.2345, Cons(2.4232, Cons(3.2345, Cons(2.4232, Cons(9.2333, Empty))))))

              Expect.equal actualResult expectedResult "The results were different"



          // Some interdependent properties of countDistinct and GeneralTree.toMyList
          testProperty "(tree |> countDistinct) should be less or equal (tree |> toMyList |> length) (int)"
          <| fun (tree: GeneralTree<int>) ->
              let input1 = tree |> countDistinct
              let input2 = tree |> toMyList |> length
              Expect.isLessThanOrEqual input1 input2

          testProperty "(tree |> countDistinct) should be less or equal (tree |> toMyList |> length) (string)"
          <| fun (tree: GeneralTree<string>) ->
              let input1 = tree |> countDistinct
              let input2 = tree |> toMyList |> length
              Expect.isLessThanOrEqual input1 input2

          testProperty "(tree |> countDistinct) should be less or equal (tree |> toMyList |> length) (float)"
          <| fun (tree: GeneralTree<float>) ->
              let input1 = tree |> countDistinct
              let input2 = tree |> toMyList |> length
              Expect.isLessThanOrEqual input1 input2

          testProperty "(tree |> countDistinct) should be less or equal (tree |> toMyList |> length) (char)"
          <| fun (tree: GeneralTree<char>) ->
              let input1 = tree |> countDistinct
              let input2 = tree |> toMyList |> length
              Expect.isLessThanOrEqual input1 input2


          testProperty "(tree |> set) should be the same as (tree |> toMyList |> toSet ) (int)"
          <| fun (tree: GeneralTree<int>) ->
              let input1 = tree |> GeneralTree.toSet
              let input2 = tree |> toMyList |> toSet
              Expect.equal input1 input2

          testProperty "(tree |> toSet) should be the same as (tree |> toMyList |> toSet ) (string)"
          <| fun (tree: GeneralTree<string>) ->
              let input1 = tree |> GeneralTree.toSet
              let input2 = tree |> toMyList |> toSet
              Expect.equal input1 input2

          testProperty "(tree |> toSet) should be the same as (tree |> toMyList |> toSet ) (float)"
          <| fun (tree: GeneralTree<NormalFloat>) ->
              let input1 = tree |> GeneralTree.toSet
              let input2 = tree |> toMyList |> toSet
              Expect.equal input1 input2

          testProperty "(tree |> toSet) should be the same as (tree |> toMyList |> toSet ) (char)"
          <| fun (tree: GeneralTree<char>) ->
              let input1 = tree |> GeneralTree.toSet
              let input2 = tree |> toMyList |> toSet
              Expect.equal input1 input2 ]
