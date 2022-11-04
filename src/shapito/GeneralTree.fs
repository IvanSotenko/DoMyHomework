module DoMyHomework.GeneralTree

open MyList

type GeneralTree<'Value> = Node of value: 'Value * children: MyList<GeneralTree<'Value>>


let treeFold (join: 'State -> 'State -> 'State) (singleton: 'A -> 'State) (state: 'State) (tree: GeneralTree<'A>) =

    let rec treeFolder (acc: 'State) (tree: GeneralTree<'A>) =
        match tree with
        | Node (v, kids) -> join acc (join (singleton v) (fold treeFolder state kids))

    match tree with
    | Node (v, kids) -> join (singleton v) (fold treeFolder state kids)


let treeFold (folder: 'State -> 'A -> 'State) (state: 'State) (tree: GeneralTree<'A>): 'State =




let toList tree =
    treeFold concat (fun v -> Cons(v, Empty)) Empty tree

let toSet tree =
    treeFold Set.union Set.empty.Add Set.empty tree

let countDistinct tree = tree |> toSet |> Set.count
