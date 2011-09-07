open System

let cubeRoot (n:int64) = Math.Pow(double(n), 1.0/3.0)

// define function to investigate the numbers of d digits which are cubes
let f d =
    // find the min & max number whose cube is d digits long
    let min, max = int64(cubeRoot (pown 10L d)), int64(cubeRoot (pown 10L (d+1)))

    // group numbers which are cubes and are permutations of one another
    // and look for groups which contains 5 numbers
    [min..max]
    |> List.map (fun n -> pown n 3)
    |> Seq.groupBy (fun n -> n.ToString().ToCharArray() |> Array.sort)
    |> Seq.filter (fun (k, l) -> l |> Seq.length = 5)

let answer =
    // go through the numbers of a given number of digits and find the
    // first set of groups of 5 numbers which are cubes and permutations
    // of one another
    let groups = 
        Seq.unfold (fun state -> Some(state, state+1)) 7
        |> Seq.map f
        |> Seq.filter (fun l -> Seq.length l > 0)
        |> Seq.head

    // find the smallest elements in the groups
    groups |> Seq.map (fun (k, l) -> l |> Seq.min) |> Seq.min