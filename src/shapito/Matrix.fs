module DoMyHomework.Matrix

open QTree


let constructQTree (basis: Option<'A> [,]) =

    let rows = Array2D.length1 basis
    let columns = Array2D.length2 basis
    let size = max rows columns

    let extract (x, y) =
        if x < rows && y < columns then
            let element = basis[x, y]

            match element with
            | Some a -> Leaf a
            | None -> Empty
        else
            Empty

    if size = 0 then
        Empty
    elif size = 1 then
        extract (0, 0)
    else

        let depth = int (System.Math.Ceiling(System.Math.Log(size, 2)))

        let rec constructSub level (x, y) =

            if level = 1 then
                Node(
                    extract (x * 2, y * 2),
                    extract (x * 2, y * 2 + 1),
                    extract (x * 2 + 1, y * 2),
                    extract (x * 2 + 1, y * 2 + 1)
                )
                |> qCollapse

            else
                Node(
                    (constructSub (level - 1) (x * 2, y * 2)),
                    (constructSub (level - 1) (x * 2, y * 2 + 1)),
                    (constructSub (level - 1) (x * 2 + 1, y * 2)),
                    (constructSub (level - 1) (x * 2 + 1, y * 2 + 1))
                )
                |> qCollapse

        collapseQTree (constructSub depth (0, 0))


let constructQTree2 (basis: Option<'A> [,]) =

    let rows = Array2D.length1 basis
    let columns = Array2D.length2 basis
    let size = max rows columns

    let extract (x, y) =
        if x < rows && y < columns then
            let element = basis[x, y]

            match element with
            | Some a -> Leaf a
            | None -> Empty
        else
            Empty

    if size = 0 then
        Empty
    elif size = 1 then
        extract (0, 0)
    else

        let depth = int (System.Math.Ceiling(System.Math.Log(size, 2)))

        let rec constructSub level (x, y) =

            if level = 1 then
                let quadNW = extract (x * 2, y * 2)
                let quadNE = extract (x * 2, y * 2 + 1)
                let quadSW = extract (x * 2 + 1, y * 2)
                let quadSE = extract (x * 2 + 1, y * 2 + 1)
                Node(quadNW, quadNE, quadSW, quadSE)

            else
                Node(
                    (constructSub (level - 1) (x * 2, y * 2)),
                    (constructSub (level - 1) (x * 2, y * 2 + 1)),
                    (constructSub (level - 1) (x * 2 + 1, y * 2)),
                    (constructSub (level - 1) (x * 2 + 1, y * 2 + 1))
                )

        collapseQTree (constructSub depth (0, 0))


type Matrix<'A when 'A: equality> =
    val Data: QTree<'A>
    val Length1: int
    val Length2: int

    new(arr) =
        { Data = constructQTree arr
          Length1 = Array2D.length1 arr
          Length2 = Array2D.length2 arr }

    new(tree, length1, length2) =
        { Data = tree
          Length1 = length1
          Length2 = length2 }

    member this.Item
        with get (x, y) =

            let size =
                int (
                    2.
                    ** (System.Math.Ceiling(System.Math.Log(max this.Length1 this.Length2, 2)))
                )

            if (x >= this.Length1) || (y >= this.Length2) then
                failwith "Matrix index out of range"
            else
                let rec find tree (curX, curY) barrier =

                    match tree with
                    | Node (nw, ne, sw, se) ->
                        if x <= (curX - barrier) && y <= (curY - barrier) then
                            find nw (curX - barrier, curY - barrier) (barrier / 2)

                        elif x <= (curX - barrier) && y > (curY - barrier) then
                            find ne (curX - barrier, curY) (barrier / 2)

                        elif x > (curX - barrier) && y <= (curY - barrier) then
                            find sw (curX, curY - barrier) (barrier / 2)

                        else
                            find se (curX, curY) (barrier / 2)

                    | Leaf a -> Some a
                    | Empty -> None

                find this.Data (size - 1, size - 1) (size / 2)
