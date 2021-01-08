open System

[<Struct>]
type Suit = Diamonds | Hearts | Spades | Clubs
[<Struct>]
type Card = Card of Suit:Suit * Val:int
module Card = 
    let suit (Card (suit, _)) = suit
    let value (Card (_, value)) = value
[<Struct>]
type Hand = Hand of Card array
[<Struct>]
type Players = P1 | P2

let getHands line = 
    Hand([|Card(Diamonds, 7); Card(Clubs, 11); Card(Spades, 2); Card(Hearts, 3); Card(Clubs, 8)|]), Hand([|Card(Diamonds, 7); Card(Clubs, 12); Card(Spades, 2); Card(Hearts, 3); Card(Clubs, 8)|])

let calcWinner h1 h2 = 
    let (|RoyalFlush|_|) (Hand cards) =
        match cards |> Array.distinctBy Card.suit with
        | suits when suits.Length = 1 ->
            match cards |> Array.map Card.value |> Array.sort with
            | [|10; 11; 12; 13; 14|] -> Some suits.[0]
            | _ -> None
        | _ -> None

    let (|StraightFlush|_|) (Hand cards) =
        match cards |> Array.distinctBy Card.suit with
        | suits when suits.Length = 1 ->
            match cards |> Array.map Card.value |> Array.sort with
            | [|x; _; _; _; y|] when y - x = 4 -> Some y
            | _ -> None
        | _ -> None

    let (|ForuOfAKind|_|) (Hand cards) =
        match cards |> Array.distinctBy Card.suit with
        | suits when suits.Length = 1 ->
            match cards |> Array.map Card.value |> Array.sort with
            | [|x; _; _; _; y|] when y - x = 4 -> Some y
            | _ -> None
        | _ -> None
    
    let rank hand = 
        match hand with
        | RoyalFlush _ -> 10, 10
        | StraightFlush highest -> 9, highest

    let rank1, subrank1 = rank h1
    let rank2, subrank2 = rank h2
    match rank1 * 100 + subrank1, rank2 * 100 + subrank2 with
    | score1, score2 when score1 > score2 -> P1
    | score1, score2 when score1 < score2 -> P2
    | _ -> failwith "Tie"

[<EntryPoint>]
let main argv =
    let rec loop winners1 winners2 = 
        let line = Console.ReadLine()
        if String.IsNullOrWhiteSpace(line) then
            printfn "Player 1: %d" winners1
            printfn "Player 2: %d" winners2
            0
        else
            match getHands line ||> calcWinner with
            | P1 -> loop (winners1 + 1) winners2
            | P2 -> loop winners1 (winners2 + 1)
    try
        loop 0 0
    with x -> printfn "An error has occurred %s" x.Message; -1
