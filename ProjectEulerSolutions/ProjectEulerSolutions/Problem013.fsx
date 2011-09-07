open System.IO
open System.Numerics
 
let sum =
    File.ReadAllLines(@"C:\TEMP\euler13.txt")
    |> Seq.map BigInteger.Parse
    |> Seq.sum
 
let firstTenDigits = sum.ToString().ToCharArray() |> Seq.take(10) |> Seq.toList