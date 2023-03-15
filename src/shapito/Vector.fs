module DoMyHomework.Vector

open BinTree

type Vector<'A when 'A: equality> =
    val Data: BinTree<'A>
    val Length: int

    new(arr: Option<'A> []) =
        { Data = init arr.Length (fun i -> arr[i])
          Length = arr.Length }

    new(tree: BinTree<'A>, length) = { Data = tree; Length = length }

    member this.IsEmpty = this.Data = Empty

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


let map2 (mapping: Option<'A> -> Option<'B> -> Option<'C>) (vector1: Vector<'A>) (vector2: Vector<'B>) : Vector<'C> =
    if vector1.Length <> vector2.Length then
        failwith "Map2 cannot be executed: vector lengths do not match"
    else
        Vector(addBinTree vector1.Data vector2.Data mapping, vector1.Length)

let pMap2 (mapping: Option<'A> -> Option<'B> -> Option<'C>) (vector1: Vector<'A>) (vector2: Vector<'B>) (pLevel: int) : Vector<'C> =
    if vector1.Length <> vector2.Length then
        failwith "Map2 cannot be executed: vector lengths do not match"
    else
        Vector(parallelAddBinTree vector1.Data vector2.Data mapping pLevel, vector1.Length)
