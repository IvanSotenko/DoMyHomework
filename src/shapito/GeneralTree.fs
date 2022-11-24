module DoMyHomework.GeneralTree

open MyList

type GeneralTree<'Value> = Node of value: 'Value * children: MyList<GeneralTree<'Value>>

let rec treeFold (folder: 'State -> 'A -> 'State) (state: 'State) (tree: GeneralTree<'A>) : 'State =
    match tree with
    | Node (v, kids) -> folder (foldRev (treeFold folder) state kids) v


let toList tree =
    treeFold (fun acc v -> Cons(v, acc)) Empty tree

let toSet tree =
    treeFold (fun acc v -> Set.add v acc) Set.empty tree

let countDistinct tree = tree |> toSet |> Set.count
