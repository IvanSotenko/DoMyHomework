module shapito.Multiply
open Matrix
open Vector

let naiveMultiply (vec: Vector<'value>) (mat: Matrix<'value>) add multiply =

    if vec.actualLength <> mat.actualLen1
    then
        failwith "Vector and matrix can't be multiplied. The length of
                  the vector does not coincide with the number of rows of the matrix"
    else
        let columns = mat.actualLen2
        let len = vec.actualLength

        let mutable newVector = [||]
        for i in 0 .. (columns - 1) do

            let m1, m2 =
                match (vec.getItem 0), (mat.getItem (0, i)) with
                | Some a, Some b -> a, b
                | _ -> failwith "some exception"

            let mutable el = multiply m1 m2

            for j in 1 .. (len - 1) do

                let m1, m2 =
                    match (vec.getItem j), (mat.getItem (j, i)) with
                    | Some a, Some b -> a, b
                    | _ -> failwith "some exception"

                el <- add el (multiply m1 m2)

            newVector <- Array.append newVector [|el|]

        Vector(newVector)

