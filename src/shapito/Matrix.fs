module DoMyHomework.Matrix

open QTree
open System.IO
open System.Collections.Generic

type Matrix<'A when 'A: equality> =
    val Data: QTree<'A>
    val Length1: int
    val Length2: int

    new(arr: Option<'A> [,]) =
        { Data = init (Array2D.length1 arr) (Array2D.length2 arr) (fun x y -> arr[x, y])
          Length1 = Array2D.length1 arr
          Length2 = Array2D.length2 arr }

    new(tree: QTree<'A>, length1, length2) =
        { Data = tree
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


let readMtxMatrix (path: string) (converter: string -> 'A): Matrix<'A> =

    if FileInfo(path).Extension <> ".mtx" then
        failwith $"Incorrect matrix path: file has wrong extension ({FileInfo(path).Extension})"

    if not (File.Exists(path)) then
        failwith $"Incorrect matrix path: file does not exist ({path}) (curDir = {Directory.GetCurrentDirectory()})"

    let lines = File.ReadAllLines(path)
    let dict = Dictionary<int * int, 'A>()
    let mutable descriptionOver = false
    let mutable length1, length2 = 0, 0

    for str in lines do
        if str[0] <> '%' then
            let splittedStr = str.Split(' ')

            if descriptionOver then
                dict.Add(((int splittedStr[0], int splittedStr[1]), converter splittedStr[2]))

            else
                length1 <- int splittedStr[0]
                length2 <- int splittedStr[1]
                descriptionOver <- true

    let initializer (x: int) (y: int) =
        if dict.ContainsKey((x + 1, y + 1)) then
            Some(dict[(x + 1, y + 1)])
        else
            None

    Matrix(init length1 length2 initializer, length1, length2)


let readMtxMatrixRec (path: string) (toType: string -> 'A): Matrix<'A> =

    if FileInfo(path).Extension <> ".mtx" then
        failwith $"Incorrect matrix path: file has wrong extension ({FileInfo(path).Extension})"

    if not (File.Exists(path)) then
        failwith $"Incorrect matrix path: file does not exist ({path}) (curDir = {Directory.GetCurrentDirectory()})"

    let lines = File.ReadAllLines(path)
    let len = lines.Length
    let dict = Dictionary<int * int, 'A>()

    let rec dictConstructor length1 length2 descriptionOver i =
        if i + 1 > len then
            length1, length2
        else
            let str = lines[i]

            match str[0] with
            | '%' -> dictConstructor length1 length2 false (i + 1)

            | _ when descriptionOver ->
                let splittedStr = str.Split(' ')
                dict.Add(((int splittedStr[0], int splittedStr[1]), toType splittedStr[2]))
                dictConstructor length1 length2 true (i + 1)

            | _ ->
                let splittedStr = str.Split(' ')
                let newLength1, newLength2 = int splittedStr[0], int splittedStr[1]
                dictConstructor newLength1 newLength2 true (i + 1)


    let initializer (x: int) (y: int) =
        if dict.ContainsKey((x + 1, y + 1)) then
            Some(dict[(x + 1, y + 1)])
        else
            None

    let length1, length2 = dictConstructor 0 0 false 0
    Matrix(init length1 length2 initializer, length1, length2)
