// define function to get the continued fractions, a0,a1,a2,...an
let continuedFraction D =
    // define functions for working out the nth term of P, Q and a sequence
    let getPn an' qn' pn' = an' * qn' - pn'
    let getQn D pn qn' = (D - pown pn 2) / qn'
    let getAn a0 pn qn = (a0 + pn) / qn
  
    // work out the initial terms
    let a0 = bigint(sqrt(double(D)))
    let p1, q1 = a0, D-a0*a0
    let a1 = getAn a0 p1 q1
    let initial = (p1, q1, a1)

    Seq.unfold (fun (pn', qn', an') ->
        let pn = getPn an' qn' pn'
        let qn = getQn D pn qn'
        let an = getAn a0 pn qn
        Some((pn', qn', an'), (pn, qn, an))) initial
    |> Seq.map (fun (pn, qn, an) -> an)
    |> Seq.append [a0]

// define function to get the continued fraction convergents
// e.g. for D = 7: 2/1, 3/1, 5/2, 8/3, ...
let continuedFractionConvergents D =
    let getN an n' n'' = an * n' + n''

    // work out the initial terms
    let fractions = continuedFraction D
    let a0 = Seq.head fractions
    let p0, p1 = a0, a0 * (Seq.nth 1 fractions) + 1I
    let q0, q1 = 1I, Seq.nth 1 fractions
    let initial = (p1, q1, p0, q0)

    Seq.scan (fun (pn', qn', pn'', qn'') an ->
        let pn = getN an pn' pn''
        let qn = getN an qn' qn''
        (pn, qn, pn', qn')) initial (fractions |> Seq.skip 2)
    |> Seq.map (fun (pn, qn, pn', qn') -> (pn, qn))
    |> Seq.append [(p0, q0)]
    
let answer = 
    [1I..1000I] 
    |> List.filter (fun d -> sqrt(double(d)) % 1.0 <> 0.0)
    |> List.maxBy (fun d -> 
        continuedFractionConvergents d
        |> Seq.filter (fun (x, y) -> x*x - d*y*y = 1I)
        |> Seq.map fst
        |> Seq.head)