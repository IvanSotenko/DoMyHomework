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


let genlst =
    let rec lst' s =
        match (s + 10) with
        | 0 -> gen { return Empty }
        | n when n > 0 ->
            let sublst = lst' (n - 1)
            Gen.map2 (fun x y -> Cons (x, y)) Arb.generate<int> sublst
        | _ -> invalidArg "s" "Only positive arguments are allowed"
    Gen.sized lst'
    // Gen.sized <| fun s -> Gen.resize (s*10) lst`

type MyGenerators =
  static member MyList() =
      {new Arbitrary<MyList<int>>() with
          override x.Generator = genlst
          override x.Shrinker t = Seq.empty }


Arb.register<MyGenerators>() |> ignore

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
