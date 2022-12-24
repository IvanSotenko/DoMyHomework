﻿module DoMyHomework.Vector

open BinTree

let vertListToVector (verts: list<int * 'A>) (length: int) =

    let value (_, a) = a
    let lengthPow2 = int (2. ** System.Math.Ceiling(System.Math.Log(length, 2)))

    let divideIntoСhildren (verts: list<int * 'A>) (curI: int) =

        let rec divideSub verts (left, right) =
            match verts with
            | (i, a) :: tail ->

                if i > length then
                    failwith
                        $"An element outside the bounds of the vector. The length of the vector is {length} but there is an element ({i}, {a})."

                if i <= curI then
                    divideSub tail ((i, a) :: left, right)
                else
                    divideSub tail (left, (i, a) :: right)

            | [] -> left, right

        divideSub verts ([], [])

    let rec constructSub (verts: list<int * 'A>) curI barrier =
        if verts.IsEmpty then
            Empty
        elif barrier = 0 then
            if verts.Length = 1 then
                Leaf(value verts[0])
            else
                failwith $"Several elements claim one place in the tree: {verts}"
        else
            let vertsLeft, vertsRight = divideIntoСhildren verts (curI - barrier)
            printfn "%A\n%A\n%A\n" vertsLeft vertsRight curI

            Node (
                constructSub vertsLeft (curI - barrier) (barrier/2),
                constructSub vertsRight curI (barrier/2)
            )
            |> binCollapse

    constructSub verts lengthPow2 (lengthPow2/2)


let constructBinTree (basis: array<Option<'A>>) =

    let length = Array.length basis

    let extract ind =
        if ind < length then
            match basis[ind] with
            | Some a -> Leaf a
            | None -> Empty
        else
            Empty

    if length = 0 then
        Empty
    elif length = 1 then
        extract 0
    else

        let depth = int (System.Math.Ceiling(System.Math.Log(length, 2)))

        let rec constructSub level i =

            if level = 1 then
                let left = extract (i * 2)
                let right = extract (i * 2 + 1)

                Node(left, right) |> binCollapse
            else
                let left = (constructSub (level - 1) (i * 2)) |> binCollapse

                let right =
                    (constructSub (level - 1) (i * 2 + 1))
                    |> binCollapse

                Node(left, right) |> binCollapse

        constructSub depth 0


type Vector<'A when 'A: equality> =
    val Data: BinTree<'A>
    val Length: int

    new(arr) =
        { Data = constructBinTree arr
          Length = arr.Length }

    new(tree, length) = { Data = tree; Length = length }

    member this.Item
        with get i =

            let len =
                int (
                    2.
                    ** System.Math.Ceiling(System.Math.Log(this.Length, 2))
                )

            if i >= this.Length then
                failwith "Vector index out of range"
            else

                let rec find tree i curI barrier =
                    match tree with
                    | Node (left, right) ->
                        if i > (curI - barrier) then
                            find right i curI (barrier / 2)
                        else
                            find left i (curI - barrier) (barrier / 2)
                    | Leaf a -> Some a
                    | Empty -> None

                find this.Data i (len - 1) (len / 2)
