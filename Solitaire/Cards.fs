module Solitaire.Cards

type Suit =
    | Spade = 0
    | Heart = 1
    | Club = 2
    | Diamond = 3

type ColorOfSuit =
    | Black = 0
    | Red = 1

type Card =
    | NumberCard of suit: Suit * number: int
    | Joker

    member this.Color =
        match this with
        | NumberCard (s, _) -> Some(int (s) % 2 |> enum<ColorOfSuit>)
        | Joker -> None

    member this.IsAce =
        match this with
        | NumberCard (_, n) -> if n = 1 then true else false
        | Joker -> false

    member this.IsKing =
        match this with
        | NumberCard (_, n) -> if n = 13 then true else false
        | Joker -> false


let toNumberCard n =
    NumberCard(suit = (n % 4 |> enum<Suit>), number = n / 4 + 1)

let createDeck jokers =
    [ for n = 0 to 51 do
          toNumberCard n ]
    @ [ for _ = 0 to jokers - 1 do
            Joker ]
