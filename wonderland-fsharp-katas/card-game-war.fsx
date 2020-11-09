// See the file card-game.md for detailed information.

// feel free to use these cards or use your own data structure

type Suit =
    | Spade
    | Club
    | Diamond
    | Heart

type Rank =
    | Value of int
    | Jack
    | Queen
    | King
    | Ace

type Card = Suit * Rank

let playRound (card1:Card,card2:Card) =      
    let compareRank (card1:Card, card2:Card): Card option =
        let rank1Value = match card1.[1] with
            | Value v -> v
            | Jack j -> 10
            | Queen q -> 11
            | King k -> 12
            | Ace a -> 13

        let rank2Value = match card2.[1] with
            | Value v -> v
            | Jack j -> 10
            | Queen q -> 11
            | King k -> 12
            | Ace a -> 13

        if rank1Value < rank2Value then
            card2
        elif rank1Value > rank2Value then
            card1
        else
            None
    
    let compareSuit (card1:Card, card2:Card): Card option =
        let suit1Value = match card1.[0] with
            | Spade s -> 0
            | Club c -> 1
            | Diamond d -> 2
            | Heart h -> 3

        let suit2Value = match card2.[0] with
            | Spade s -> 0
            | Club c -> 1
            | Diamond d -> 2
            | Heart h -> 3

        if suit1Value < suit2Value then
            card2
        elif suit1Value > suit2Value then
            card1
        else
            None
        
    let winningCard = compareRank(card1, card2)
    if winningCard != unit then
        winningCard
    else
        compareSuit(card1, card2)

let playGame (hand1:Card list, hand2:Card list) =
    failwith "not implemented: game winner"

(*
let suits = [ Spade; Club; Diamond; Heart ]
let heads = [ Jack; Queen; King; Ace ]

let ranks =
    [   for v in 2 .. 10 -> Value v
        for head in heads -> head
    ]

let deck = seq {
    for suit in suits do
        for rank in ranks -> suit,rank }
*)

#r @"../packages/Unquote/lib/net45/Unquote.dll"
open Swensen.Unquote

// fill in tests for your game
let tests () =

    // playRound
    test <@ playRound((Spade, Value 5), (Diamond, Jack)) = (Diamond, Jack) @>

    printfn "TODO: the highest rank wins the cards in the round"
    printfn "TODO: queens are higher rank than jacks"
    printfn "TODO: kings are higher rank than queens"
    printfn "TODO: aces are higher rank than kings"
    printfn "TODO: if the ranks are equal, clubs beat spades"
    printfn "TODO: if the ranks are equal, diamonds beat clubs"
    printfn "TODO: if the ranks are equal, hearts beat diamonds"

    // playGame
    printfn "TODO: the player loses when they run out of cards"

// run the tests
tests ()
