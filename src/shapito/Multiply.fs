module shapito.Multiply
open Matrix
open Vector

let multiply (vec: Vector) (mat: Matrix) =

    if vec.actualLength <> mat.actualRows
    then
        failwith "Vector and matrix can't be multiplied. The length of
                  the vector does not coincide with the number of rows of the matrix"
    else
        let columns = mat.actualColumns
        let len = vec.actualLength

        let mutable newVector = [||]
        for i in 0 .. (columns - 1) do
            let mutable el = 0
            for j in 0 .. (len - 1) do
                let m1, m2 =
                    match (vec.element j), (mat.element (j, i)) with
                    | Some a, Some b -> a, b
                    | _ -> failwith "some exception"

                el <- el + (m1 * m2)

            newVector <- Array.append newVector [|el|]

        Vector(newVector)

