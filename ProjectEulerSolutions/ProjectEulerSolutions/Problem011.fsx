open System.IO
 
let initArray =
    File.ReadAllLines(@"C:\TEMP\euler11.txt")
    |> Seq.map (fun l -> l.Split(' ') |> Seq.map int32 |> Seq.toArray)
    |> Seq.toArray
 
let height, width = initArray.Length, initArray |> Seq.map (fun l -> l.Length) |> Seq.max
let twoDArray = Array2D.init height width (fun i j -> initArray.[i].[j])
 
let Up (array2D:int[,]) h w n =
    let lowerBound = h-(n-1)
    let upperBound = h
    if lowerBound < 0 || upperBound > height-1 then []
    else [lowerBound..upperBound] |> List.map (fun y -> array2D.[y, w])
 
let Left (array2D:int[,]) h w n =
    let lowerBound = w-(n-1)
    let upperBound = w
    if lowerBound < 0 || upperBound > width-1 then []
    else [lowerBound..upperBound] |> List.map (fun x -> array2D.[h, x])
 
let LeftDiag (array2D:int[,]) h w n =
    let lowerWBound = w-(n-1)
    let upperWBound = w
    let lowerHBound = h-(n-1)
    let upperHBound = h
    if lowerWBound < 0 || upperWBound > width-1 || lowerHBound < 0 || upperHBound > height-1 
    then []
    else
        let wCoordinates = [lowerWBound..upperWBound]
        let hCoordinates = [lowerHBound..upperHBound]
        List.map2 (fun y x -> array2D.[y, x]) hCoordinates wCoordinates
 
let RightDiag (array2D:int[,]) h w n =
    let lowerWBound = w
    let upperWBound = w+(n-1)
    let lowerHBound = h-(n-1)
    let upperHBound = h
    if lowerWBound < 0 || upperWBound > width-1 || lowerHBound < 0 || upperHBound > height-1 
    then []
    else
        let wCoordinates = [lowerWBound..upperWBound]
        let hCoordinates = [lowerHBound..upperHBound] |> List.rev
        List.map2 (fun y x -> array2D.[y, x]) hCoordinates wCoordinates
 
let quartets =
    seq { for y in 3 .. width-1 do
            for x in 3 .. height-1 do
                yield Up twoDArray x y 4
                yield Left twoDArray x y 4
                yield LeftDiag twoDArray x y 4
                yield RightDiag twoDArray x y 4
    }
 
let CalcProduct numbers = numbers |> Seq.fold (fun acc n -> acc * n) 1
let maxProduct = quartets |> Seq.map CalcProduct |> Seq.max