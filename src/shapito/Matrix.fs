module DoMyHomework.Matrix

open QTree

let vertListToMatrix (verts: list<int * int * 'A>) (rows: int) (columns: int) =

    let value (_, _, a) = a
    let size = max rows columns

    let sizePow2 =
        int (
            2.
            ** System.Math.Ceiling(System.Math.Log(size, 2))
        )

    let divideIntoQuads (verts: list<int * int * 'A>) (curX: int) (curY: int) =

        let rec divideSub verts (nw, ne, sw, se) =
            match verts with
            | (x, y, a) :: tail ->

                if x > rows || y > columns then
                    failwith
                        $"An element outside the bounds of the matrix. The dimensions of the matrix are {rows}x{columns} but there is an element ({x}, {y}, {a})."

                if x <= curX && y <= curY then
                    divideSub tail ((x, y, a) :: nw, ne, sw, se)
                elif x <= curX && y > curY then
                    divideSub tail (nw, (x, y, a) :: ne, sw, se)
                elif x > curX && y <= curY then
                    divideSub tail (nw, ne, (x, y, a) :: sw, se)
                else
                    divideSub tail (nw, ne, sw, (x, y, a) :: se)

            | [] -> nw, ne, sw, se

        divideSub verts ([], [], [], [])

    let rec constructSub (verts: list<int * int * 'A>) (curX, curY) barrier =
        if verts.IsEmpty then
            Empty
        elif barrier = 0 then
            if verts.Length = 1 then
                Leaf(value verts[0])
            else
                failwith $"Several elements claim one place in the tree: {verts}"
        else
            let vertsNW, vertsNE, vertsSW, vertsSE =
                divideIntoQuads verts (curX - barrier) (curY - barrier)

            Node(
                constructSub vertsNW (curX - barrier, curY - barrier) (barrier / 2),
                constructSub vertsNE (curX - barrier, curY) (barrier / 2),
                constructSub vertsSW (curX, curY - barrier) (barrier / 2),
                constructSub vertsSE (curX, curY) (barrier / 2)
            )
            |> qCollapse

    constructSub verts (sizePow2, sizePow2) (sizePow2 / 2)


let array2DToQTree (basis: Option<'A> [,]) =

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

        constructSub depth (0, 0)


type Matrix<'A when 'A: equality> =
    val Data: QTree<'A>
    val Length1: int
    val Length2: int

    new(arr: Option<'A> [,]) =
        { Data = array2DToQTree arr
          Length1 = Array2D.length1 arr
          Length2 = Array2D.length2 arr }

    new(tree: QTree<'A>, length1, length2) =
        { Data = tree
          Length1 = length1
          Length2 = length2 }

    new(verts: list<int * int * 'A>, length1, length2) =
        { Data = vertListToMatrix verts length1 length2
          Length1 = length1
          Length2 = length2 }

    member this.Item
        with get (x, y) =

            let size =
                int (
                    2.
                    ** System.Math.Ceiling(System.Math.Log(max this.Length1 this.Length2, 2))
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
