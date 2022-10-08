namespace Klondike

module Klondike =

    type Suit =
        | Spade
        | Club
        | Heart
        | Diamond

    type ColorOfSuit =
        | Black
        | Red

    type Card =
        struct
            val Suit: Suit
            val Number: int
            new(suit: Suit, number: int) = { Suit = suit; Number = number }

            member this.Color =
                match this.Suit with
                | Spade -> Black
                | Club -> Black
                | Heart -> Red
                | Diamond -> Red

            member this.IsKing = if this.Number = 13 then true else false

            member this.IsAce = if this.Number = 1 then true else false

            member this.IsNextTo(card: Card) =
                if this.Number = card.Number + 1 then true else false

            member this.IsBefore(card: Card) =
                if this.Number = card.Number - 1 then true else false
        end


    let toCard value =
        let suitOf m =
            match m with
            | 0 -> Spade
            | 1 -> Club
            | 2 -> Heart
            | _ -> Diamond // 3

        Card(suitOf (value % 4), (if value = 0 then 1 else (value / 4) + 1))

    type PileStatus =
        | Empty
        | AllOpened
        | AllClosed
        | Others

    // FIXME: もっとスマートに表現できないか？
    type Pile =
        { Opened: Card array
          Closed: Card array }

        member this.Status =
            if Array.isEmpty this.Opened then
                if Array.isEmpty this.Closed then Empty else AllClosed
            else if Array.isEmpty this.Closed then
                AllOpened
            else
                Others

        member this.OpenTailOfClosed =
            match this.Status with
            | Empty -> Error "This pile is Empty"
            | AllClosed ->
                let tailOfClosed = Array.last this.Closed

                Ok(
                    { Opened = [| tailOfClosed |]
                      Closed = this.Closed[.. this.Closed.Length - 2] }
                )
            | _ -> Error "Opened cards is not Empty"

        member this.PickFromOpened count =
            if count > this.Opened.Length then
                Error "Opened cards is Empty in this Pile"
            else if count = this.Opened.Length then
                Ok(
                    this.Opened,
                    { Closed = Array.copy this.Closed
                      Opened = [||] }
                )
            else
                Ok(
                    this.Opened[this.Opened.Length - count ..],
                    { Closed = Array.copy this.Closed
                      Opened = this.Opened[.. this.Opened.Length - (count + 1)] }
                )

        member this.CanPutToOpened(card: Card) =
            let tailOfOpened = Array.last this.Opened

            match tailOfOpened.Color with
            | Black ->
                if card.Color = Red && card.IsBefore tailOfOpened then
                    true
                else
                    false
            | Red ->
                if card.Color = Black && card.IsBefore tailOfOpened then
                    true
                else
                    false

        member this.PutCards(cards: Card array) =
            if Array.isEmpty cards then
                Error "No cards to put"
            else
                let headOfCards = cards[0]

                match this.Status with
                | Empty when headOfCards.IsKing ->
                    Ok(
                        { Opened = Array.copy cards
                          Closed = [||] }
                    )
                | Empty when not headOfCards.IsKing -> Error "Cannot put the cards when head number is not 13"
                | AllClosed -> Error "Please deal the tail of Closed cards"
                | _ ->
                    if this.CanPutToOpened headOfCards then
                        Ok(
                            { Opened = Array.append this.Opened cards
                              Closed = Array.copy this.Closed }
                        )
                    else
                        Error
                            "Cannot put the cards when the number of the head card or color of the head card is mismatched"

    type Tableau =
        { Piles: Pile array }

        // TODO: 後で繰り返しに書き換える
        static member Init(cards: Card array) =
            if cards.Length < 28 then
                Error "Not enough cards"
            else
                Ok(
                    { Piles =
                        [| { Opened = [| cards[0] |]
                             Closed = [||] }
                           { Opened = [| cards[2] |]
                             Closed = [| cards[1] |] }
                           { Opened = [| cards[5] |]
                             Closed = cards[3..4] }
                           { Opened = [| cards[9] |]
                             Closed = cards[6..8] }
                           { Opened = [| cards[14] |]
                             Closed = cards[10..13] }
                           { Opened = [| cards[20] |]
                             Closed = cards[15..19] }
                           { Opened = [| cards[27] |]
                             Closed = cards[21..26] } |] },
                    cards[28..]
                )


    type Foundations =
        { InnerMap: Map<Suit, Card array> }

        member this.PutCard(card: Card) =
            let l = this.InnerMap[card.Suit]

            if
                (Array.isEmpty l && card.IsAce)
                || (not (Array.isEmpty l) && Array.last l |> card.IsNextTo)
            then
                Ok { InnerMap = this.InnerMap.Add(card.Suit, (Array.append l [| card |])) }
            else
                Error "Cannot put the card on this Suit"

        static member Init =
            { InnerMap = Map [ (Spade, [||]); (Heart, [||]); (Club, [||]); (Diamond, [||]) ] }

    // TODO: 前の状態と遷移可能な次の状態を載せる？
    type State =
        { Picked: Card array
          Stock: Card array
          Waste: Card array
          Tableau: Tableau
          Foundations: Foundations }

        static member Init cards =
            match Tableau.Init cards with
            | Error m -> Error m
            | Ok result ->
                let tableau, stock = result

                Ok
                    { Picked = [||]
                      Stock = stock
                      Waste = [||]
                      Tableau = tableau
                      Foundations = Foundations.Init }
