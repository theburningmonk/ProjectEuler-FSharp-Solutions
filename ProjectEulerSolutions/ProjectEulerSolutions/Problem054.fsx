open System.IO

// define the suits
type Suit = | Heart | Club | Spade | Diamond

// define the card values
type CardValue = 
    | ValueCard of int | Jack | Queen | King | Ace
    // define the + unary operator on the CardValue type
    static member (~+) (value:CardValue) =
        match value with
        | ValueCard n -> if n < 10 then ValueCard (n+1) else Jack
        | Jack -> Queen | Queen -> King | King -> Ace | Ace -> ValueCard(2)

// define the Card type
type Card = { Value:CardValue; Suit:Suit }

// define the ranks
type Rank = 
    | HighCard          of CardValue
    | OnePair           of CardValue
    | TwoPairs          of CardValue * CardValue
    | Three             of CardValue
    | Straight          of CardValue        // highest value card
    | Flush
    | FullHouse         of CardValue * CardValue
    | Four              of CardValue
    | StraightFlush     of CardValue        // highest value card & suit
    | RoyalFlush

// define function to check if cards are the same suit
let isSameSuit (cards:Card list) = cards |> List.forall (fun c -> c.Suit = cards.Head.Suit)

// define function to get the highest card
let getHighestCard(cards:Card list) = cards |> List.maxBy (fun card -> card.Value)

// define function to check if cards are a straight set
let isStraight (cards:Card list) =
    let flag = cards |> List.sort |> Seq.windowed 2 
               |> Seq.forall (fun [|card1; card2|] -> card2.Value = +card1.Value)
    (flag, getHighestCard cards)

// define function to get the duplicated cards
let getDupes (cards:Card list) = 
    cards 
    |> Seq.groupBy (fun card -> card.Value)
    |> Seq.map (fun (k, seq) -> (k, Seq.length seq))
    |> Seq.filter (fun (k, len) -> len > 1)
    |> Seq.toList

// evaluate the given hand of cards to return the rank
let evaluateHand (cards:Card list) =
    let (isStraight, highCard) = isStraight cards
    let sameSuit = isSameSuit cards

    if sameSuit && isStraight && highCard.Value = Ace then RoyalFlush
    else if sameSuit && isStraight then StraightFlush(highCard.Value)
    else if sameSuit then Flush
    else if isStraight then Straight(highCard.Value)
    else
        let dupes = getDupes cards
        if dupes.Length = 0 then HighCard(highCard.Value)
        else if dupes.Length = 1 then
            match dupes.[0] with
            | (cardValue,2) -> OnePair(cardValue)
            | (cardValue,3) -> Three(cardValue)
            | (cardValue,4) -> Four(cardValue)
        else
            match dupes with
            | [(cardValue',2);(cardValue'',2)] -> TwoPairs(cardValue', cardValue'')
            | [(cardValue',3);(cardValue'',2)] -> FullHouse(cardValue', cardValue'')
            | [(cardValue',2);(cardValue'',3)] -> FullHouse(cardValue'', cardValue')

// define function to check if player 1 wins
let isP1Winner (p1:Card list) (p2:Card list) =         
    let p1Rank, p2Rank = evaluateHand p1, evaluateHand p2
    
    if p1Rank > p2Rank then true
    else if p1Rank = p2Rank then
        let rec compareHighCard (p1':Card list) (p2':Card list) =
            let p1'HighCard, p2'HighCard = getHighestCard p1', getHighestCard p2'
            if p1'HighCard.Value > p2'HighCard.Value then true
            else if p1'HighCard.Value = p2'HighCard.Value then
                let p1'' = p1' |> List.filter (fun c -> c.Value < p1'HighCard.Value)
                let p2'' = p2' |> List.filter (fun c -> c.Value < p2'HighCard.Value)
                compareHighCard p1'' p2''
            else false

        compareHighCard p1 p2
    else false
        
// define function to parse, say, "3D" into a 3 of Diamond        
let parseCard [|(value:char);(suit:char)|] =
    let cardValue = 
        match value with
        | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9' -> ValueCard(int(value.ToString()))
        | 'T' -> ValueCard(10) | 'J' -> Jack | 'Q' -> Queen | 'K' -> King | 'A' -> Ace
    let suit = 
        match suit with
        | 'S' -> Spade | 'H' -> Heart | 'D' -> Diamond | 'C' -> Club
    { Card.Value=cardValue; Suit=suit }

// get the array of all the dealt hands from the poker text file
let hands = 
    File.ReadAllLines(@"C:\TEMP\poker.txt") 
    |> Array.map (fun str -> 
        str.Split(' ') |> Array.map (fun str -> parseCard (str.ToCharArray())))

// separate the hands into two different arrays, one for player 1 and the other player 2
let p1Hands = hands |> Array.map (fun cards -> cards |> Seq.take 5 |> Seq.toList)
let p2Hands = hands |> Array.map (fun cards -> cards |> Seq.skip 5 |> Seq.toList)

let answer =
    Array.map2 (fun p1 p2 -> isP1Winner p1 p2) p1Hands p2Hands   
    |> Array.filter (fun b -> b)
    |> Array.length