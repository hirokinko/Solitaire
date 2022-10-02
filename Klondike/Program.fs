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
        { Suit: Suit
          Number: int }

        member this.Color =
            match this.Suit with
            | Spade -> Black
            | Club -> Black
            | Heart -> Red
            | Diamond -> Red

        member this.IsKing = if this.Number = 13 then true else false

        member this.IsAce = if this.Number = 1 then true else false

    let toCard value =
        let suitOf m =
            match m with
            | 0 -> Spade
            | 1 -> Club
            | 2 -> Heart
            | _ -> Diamond // 3

        { Suit = suitOf (value % 4)
          Number = (value / 4) + 1 }

    type PileStatus =
        | Empty
        | AllOpened
        | AllClosed
        | Others

    // FIXME: もっとスマートに表現できないか？
    type Pile =
        { Opened: Card list
          Closed: Card list }

        member this.PickFromOpened count =
            if count > this.Opened.Length then
                Error "Opened cards is Empty in this Pile"
            else if count = this.Opened.Length then
                Ok(this.Opened, { Closed = this.Closed; Opened = [] })
            else
                Ok(
                    this.Opened[this.Opened.Length - count ..],
                    { Closed = this.Closed
                      Opened = this.Opened[.. this.Opened.Length - (count + 1)] }
                )

        member this.Status =
            if this.Opened.IsEmpty then
                if this.Closed.IsEmpty then Empty else AllClosed
            else if this.Closed.IsEmpty then
                AllOpened
            else
                Others

        member this.OpenTailOfClosed =
            match this.Status with
            | Empty -> Error "This pile is Empty"
            | AllClosed ->
                Ok(
                    { Opened = this.Closed.Tail
                      Closed = this.Closed[.. this.Closed.Length - 1] }
                )
            | _ -> Error "Opened cards is not Empty"

        member this.CanPutToOpened(card: Card) =
            let tailOfOpened = this.Opened.Tail[0]

            match tailOfOpened.Color with
            | Black ->
                if card.Color = Red && card.Number = tailOfOpened.Number - 1 then
                    true
                else
                    false
            | Red ->
                if card.Color = Black && card.Number = tailOfOpened.Number - 1 then
                    true
                else
                    false

        member this.PutCards(cards: Card list) =
            if cards.IsEmpty then
                Error "No cards to put"
            else
                let headOfCards = cards.Head

                match this.Status with
                | Empty when headOfCards.IsKing -> Ok({ Opened = cards; Closed = [] })
                | Empty when not headOfCards.IsKing -> Error "Cannot put the cards when head number is not 13"
                | AllClosed -> Error "Please deal the tail of Closed cards"
                | _ ->
                    if this.CanPutToOpened headOfCards then
                        Ok(
                            { Opened = this.Opened @ cards
                              Closed = this.Closed }
                        )
                    else
                        Error
                            "Cannot put the cards when the number of the head card or color of the head card is mismatched"

    type Tableau = { Piles: Pile list }

    // FIXME: 列ごとにスートを固定できるように型で表現したい
    type Foundations =
        { Spades: Card list
          Clubs: Card list
          Hearts: Card list
          Diamonds: Card list }

    let canPutCardToFoundation card prev =
        match prev with
        | None -> if card.Number = 1 then true else false
        | Some p ->
            if card.Suit = p.Suit && card.Number = p.Number + 1 then
                true
            else
                false

    type State =
        { Picked: Card list
          Stock: Card list
          Waste: Card list
          Tableau: Tableau
          Foundations: Foundations }
