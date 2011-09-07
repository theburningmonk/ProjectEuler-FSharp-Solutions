// recursive function that'll return all the possible scores
let rec getScores numbers acc iter max =
    if iter < max 
    then numbers |> List.collect (fun n -> getScores numbers (acc + n) (iter + 1) max)
    else numbers |> List.map (fun n -> n + acc)

// gets the possible scores and the associated probability of achieving that score
let getScoreProbs numbers count =
    // given the set of possible numbers on each dice and the number of dice
    // what's the prob of getting each permutation (1, 1, 2, 3)
    let permutProb = 1.0 / float(numbers |> List.length) ** float(count)

    // get all the permutation of the dices and the the corresponding score
    // and then group the occurance of each possible score and work out
    // the possibility of achieving each score
    (getScores numbers 0 1 count) 
    |> Seq.groupBy (fun n -> n)
    |> Seq.map (fun gr -> (fst gr, permutProb * float((snd gr) |> Seq.length)))
    |> Seq.toList

// work out the probability of peter and colin getting each of the possible scores   
let peterScoreProbs = getScoreProbs [1..4] 9
let colinScoreProbs = getScoreProbs [1..6] 6

let answer =
    peterScoreProbs
    |> List.map (fun gr -> 
        colinScoreProbs 
        |> List.filter (fun gr' -> (fst gr') < (fst gr)) 
        |> List.map (fun gr' -> (snd gr') * (snd gr))
        |> List.sum)
    |> List.sum