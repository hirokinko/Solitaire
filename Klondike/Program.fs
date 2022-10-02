module Klondike.Game =

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

    let toCard value =
        let suitOf m =
            match m with
            | 0 -> Spade
            | 1 -> Club
            | 2 -> Heart
            | _ -> Diamond // 3

        { Suit = suitOf (value % 4)
          Number = (value / 4) + 1 }

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

        member this.OpenTailOfClosed =
            if not this.Opened.IsEmpty then
                Error "Opened cards is not Empty"
            else if this.Opened.IsEmpty && this.Closed.IsEmpty then
                Error "This pile is Empty"
            else
                Ok(
                    { Opened = this.Closed.Tail
                      Closed = this.Closed[.. this.Closed.Length - 1] }
                )

    let canPutCardToPile card prev =
        match prev with
        | None -> if card.Number = 13 then true else false
        | Some (p: Card) ->
            match p.Color with
            | Black ->
                if card.Color = Red && card.Number = p.Number - 1 then
                    true
                else
                    false
            | Red ->
                if card.Color = Black && card.Number = p.Number - 1 then
                    true
                else
                    false

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
