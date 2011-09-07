let rec distribute e = function
    | [] -> [[e]]
    | x::xs' as xs -> (e::xs)::[for xs in distribute e xs' -> x::xs]
 
let rec permute = function
    | [] -> [[]]
    | e::xs -> List.collect (distribute e) (permute xs)
 
let getLexicographic (list:bigint list) =
    list
    |> permute
    |> List.map (fun l -> l |> List.rev |> List.mapi (fun i e -> e * pown 10I i) |> List.sum)
    |> List.sort
 
let answer = List.nth (getLexicographic [0I..9I]) (1000000-1)