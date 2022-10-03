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

        member this.IsNextTo card =
            if this.Number = card.Number + 1 then true else false

        member this.IsBefore card =
            if this.Number = card.Number - 1 then true else false

    let toCard value =
        let suitOf m =
            match m with
            | 0 -> Spade
            | 1 -> Club
            | 2 -> Heart
            | _ -> Diamond // 3

        { Suit = suitOf (value % 4)
          Number = if value = 0 then 1 else (value / 4) + 1 }

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
                let tailOfClosed = this.Closed[this.Closed.Length - 1]

                Ok(
                    { Opened = [| tailOfClosed |]
                      Closed = this.Closed[.. this.Closed.Length - 2] }
                )
            | _ -> Error "Opened cards is not Empty"

        member this.PickFromOpened count =
            if count > this.Opened.Length then
                Error "Opened cards is Empty in this Pile"
            else if count = this.Opened.Length then
                Ok(this.Opened, { Closed = this.Closed; Opened = [||] })
            else
                Ok(
                    this.Opened[this.Opened.Length - count ..],
                    { Closed = this.Closed
                      Opened = this.Opened[.. this.Opened.Length - (count + 1)] }
                )

        member this.CanPutToOpened(card: Card) =
            let tailOfOpened = this.Opened[this.Opened.Length - 1]

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
                | Empty when headOfCards.IsKing -> Ok({ Opened = cards; Closed = [||] })
                | Empty when not headOfCards.IsKing -> Error "Cannot put the cards when head number is not 13"
                | AllClosed -> Error "Please deal the tail of Closed cards"
                | _ ->
                    if this.CanPutToOpened headOfCards then
                        Ok(
                            { Opened = Array.append this.Opened cards
                              Closed = this.Closed }
                        )
                    else
                        Error
                            "Cannot put the cards when the number of the head card or color of the head card is mismatched"

    type Tableau = { Piles: Pile array }

    type Foundations =
        { InnerMap: Map<Suit, Card list> }

        member this.PutCard(card: Card) =
            let l = this.InnerMap[card.Suit]

            if (l.IsEmpty && card.IsAce) || (not l.IsEmpty && card.IsNextTo l.Tail[0]) then
                Ok { InnerMap = this.InnerMap.Add(card.Suit, (l @ [ card ])) }
            else
                Error "Cannot put the card on this Suit"

    type State =
        { Picked: Card array
          Stock: Card array
          Waste: Card array
          Tableau: Tableau
          Foundations: Foundations }
