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
          // Conversions
          testProperty ""
          <| fun (tree: GeneralTree<int>) ->
              // printfn $"{tree}"
              let result = SetOfGeneralTree tree
              let result2 = tree |> GeneralTreeToList |> SetOfMyList
              Expect.equal result result2 "The results were different"

          testProperty "list"
          <| fun (lst: MyList<int>) ->
              printfn $"{lst}"
              Expect.equal 1 1 "Lol"
        ]
