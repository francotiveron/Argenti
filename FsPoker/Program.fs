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

let mutable sss = ""

let getHands (line:string) = 
    let toHand cards = 
        let toCard (card:string) = 
            let toSuit = function | 'D' -> Diamonds | 'H' -> Hearts | 'S' -> Spades | 'C' -> Clubs | suit -> failwithf "Wrong suit %c" suit
            let toValue = function
                | char when List.contains char ['2'..'9'] -> (int char) - (int '0')
                | 'T' -> 10 | 'J' -> 11 | 'Q' -> 12 | 'K' -> 13 | 'A' -> 14
                | value -> failwithf "Wrong value %c" value

            (toSuit card.[1], toValue card.[0]) |> Card
        cards |> Array.map toCard |> Hand
        
    match line.Split " " with
    | cards when cards.Length = 10 && cards |> Array.forall (String.length >> ((=) 2)) -> toHand cards.[..4], toHand cards.[5..]
    | _ -> failwith "Wrong Input"

let calcWinner h1 h2 = 
    let (|Distinct|Pair|TwoPairs|ThreeOfAKind|FullHouse|FourOfAKind|) (Hand cards) =
        let sameSuit = if cards |> Array.forall (Card.suit >> ((=) (Card.suit cards.[0]))) then 1 else 0

        match cards |> Array.groupBy Card.value |> Array.sortByDescending (fun (_, cards) -> cards.Length) with
        | [|(value1, cards1); (value2, _)|] when cards1.Length = 4 -> FourOfAKind (value1, [|value2|])
        | [|(value1, cards1); (value2, _)|] when cards1.Length = 3 -> FullHouse (value1, [|value2|])
        | [|(value1, cards1); (value2, _); (value3, _)|] when cards1.Length = 3 -> ThreeOfAKind (value1, [|value2; value3|])
        | [|(value1, cards1); (value2, _); (value3, _)|] when cards1.Length = 2 -> TwoPairs (max value1 value2, [|min value1 value2; value3|])
        | [|(value1, cards1); (value2, _); (value3, _); (value4, _)|] when cards1.Length = 2 -> Pair (value1, [|value2; value3; value4|])
        | _ -> 
            let ordered = cards |> Array.map Card.value |> Array.sortDescending
            Distinct (sameSuit, ordered)

    let score = function
        | Distinct (sameSuit, ordered) ->
            if (ordered.[0] - ordered.[4] = 4) then //Straight, Straight Flush or Royal Flush
                if sameSuit = 1 then //Straight Flush or Royal Flush
                    if ordered.[0] = 14 then //Royal Flush
                        1000, [||]
                    else //Straight Flush
                        900 + ordered.[0], [||]
                else //Straight
                    500 + ordered.[0], [||]
            else //Flush or High Card
                if sameSuit = 1 then //Flush
                    600 + ordered.[0], ordered.[1..]
                else //High Card
                    100 + ordered.[0], ordered.[1..]
        | Pair (pairValue, remaining3Ordered) -> 200 + pairValue, remaining3Ordered
        | TwoPairs (highestPairValue, remaining1) -> 300 + highestPairValue, remaining1
        | ThreeOfAKind (trisValue, remaining2) -> 400 + trisValue, remaining2
        | FullHouse (trisValue, pairValue) -> 700 + trisValue, pairValue
        | FourOfAKind (pokerValue, remaining1) -> 800 + pokerValue, remaining1

    match score h1, score h2 with
    | (score1, _), (score2, _) when score1 > score2 -> P1
    | (score1, _), (score2, _) when score1 < score2 -> P2
    | (_, rem1), (_, rem2) ->
        match Array.sortDescending rem1, Array.sortDescending rem2 with
        | ord1, ord2 when ord1 > ord2 -> sss <- sss + "1"; P1
        | ord1, ord2 when ord1 < ord2 -> sss <- sss + "2"; P2
        | _ -> failwith "Tie"

[<EntryPoint>]
let main _ =
    let rec loop winners1 winners2 = 
        let line = Console.ReadLine()
        if String.IsNullOrWhiteSpace(line) then //EOF
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