namespace Klondike.Tests

open NUnit.Framework
open Klondike.Klondike


[<TestFixture>]
type CardTestClass() =

    [<Test>]
    member this.TestIsKing() =
        let card = { Suit = Spade; Number = 13 }
        Assert.True(card.IsKing)

    [<Test>]
    member this.TestIsNotKing() =
        let card = { Suit = Spade; Number = 12 }
        Assert.False(card.IsKing)

    [<Test>]
    member this.TestIsAce() =
        let card = { Suit = Spade; Number = 1 }
        Assert.True(card.IsAce)

    [<Test>]
    member this.TestIsNotAce() =
        let card = { Suit = Spade; Number = 2 }
        Assert.False(card.IsAce)

    [<Test>]
    member this.TestColorOfSpadeIsBlack() =
        let card = { Suit = Spade; Number = 1 }
        Assert.AreEqual(Black, card.Color)

    [<Test>]
    member this.TestColorOfClubIsBlack() =
        let card = { Suit = Club; Number = 1 }
        Assert.AreEqual(Black, card.Color)

    [<Test>]
    member this.TestColorOfHeartIsRed() =
        let card = { Suit = Heart; Number = 1 }
        Assert.AreEqual(Red, card.Color)

    [<Test>]
    member this.TestColorOfDiamondIsRed() =
        let card = { Suit = Diamond; Number = 1 }
        Assert.AreEqual(Red, card.Color)

    [<Test>]
    member this.Test2IsNextTo1() =
        let card = { Suit = Spade; Number = 2 }
        Assert.True(card.IsNextTo({ Suit = Spade; Number = 1 }))

    [<Test>]
    member this.Test2IsNotNextTo2() =
        let card = { Suit = Spade; Number = 2 }
        Assert.False(card.IsNextTo({ Suit = Spade; Number = 2 }))

    [<Test>]
    member this.Test2IsNotNextTo3() =
        let card = { Suit = Spade; Number = 2 }
        Assert.False(card.IsNextTo({ Suit = Spade; Number = 3 }))

    [<Test>]
    member this.TestIsNotNextToWhenItIsNotSequential() =
        let card = { Suit = Spade; Number = 3 }
        Assert.False(card.IsNextTo({ Suit = Spade; Number = 1 }))

    [<Test>]
    member this.Test1IsBefore2() =
        let card = { Suit = Spade; Number = 1 }
        Assert.True(card.IsBefore({ Suit = Spade; Number = 2 }))

    [<Test>]
    member this.Test1IsNotBefore1() =
        let card = { Suit = Spade; Number = 1 }
        Assert.False(card.IsBefore({ Suit = Spade; Number = 1 }))

    [<Test>]
    member this.Test2IsNotBefore1() =
        let card = { Suit = Spade; Number = 2 }
        Assert.False(card.IsBefore({ Suit = Spade; Number = 1 }))

    [<Test>]
    member this.TestIsNotBeforeWhenItIsNotSequential() =
        let card = { Suit = Spade; Number = 1 }
        Assert.False(card.IsNextTo({ Suit = Spade; Number = 3 }))

    // TODO: 0 〜 51 までの各値のテストを追加
    [<Test>]
    member this.TestToCard() =
        let value1 = 0
        Assert.AreEqual({ Suit = Spade; Number = 1 }, toCard value1)

        let value2 = 1
        Assert.AreEqual({ Suit = Club; Number = 1 }, toCard value2)

        let value3 = 2
        Assert.AreEqual({ Suit = Heart; Number = 1 }, toCard value3)

        let value4 = 3
        Assert.AreEqual({ Suit = Diamond; Number = 1 }, toCard value4)

        let value5 = 48
        Assert.AreEqual({ Suit = Spade; Number = 13 }, toCard value5)

        let value6 = 49
        Assert.AreEqual({ Suit = Club; Number = 13 }, toCard value6)

        let value7 = 50
        Assert.AreEqual({ Suit = Heart; Number = 13 }, toCard value7)

        let value8 = 51
        Assert.AreEqual({ Suit = Diamond; Number = 13 }, toCard value8)

[<TestFixture>]
type PileTestClass() =

    [<Test>]
    member this.TestStatusIsEmpty() =
        let pile = { Opened = [||]; Closed = [||] }
        Assert.AreEqual(Empty, pile.Status)

    [<Test>]
    member this.TestStatusIsAllClosed() =
        let pile =
            { Opened = [||]
              Closed = [| { Suit = Spade; Number = 1 } |] }

        Assert.AreEqual(AllClosed, pile.Status)

    [<Test>]
    member this.TestStatusIsAllOpened() =
        let pile =
            { Opened = [| { Suit = Spade; Number = 1 } |]
              Closed = [||] }

        Assert.AreEqual(AllOpened, pile.Status)

    [<Test>]
    member this.TestStatusIsOthers() =
        let pile =
            { Opened = [| { Suit = Spade; Number = 1 } |]
              Closed = [| { Suit = Spade; Number = 2 } |] }

        Assert.AreEqual(Others, pile.Status)

    [<Test>]
    member this.TestOpenTailOfClosedIfPileIsEmpty() =
        let pile = { Opened = [||]; Closed = [||] }

        match pile.OpenTailOfClosed with
        | Error m -> Assert.AreEqual("This pile is Empty", m)
        | _ -> Assert.Fail("Invalid result")

    [<Test>]
    member this.TestOpenTailOfClosedIfPileIsAllClosed() =
        let pile =
            { Opened = [||]
              Closed = [| { Suit = Spade; Number = 1 } |] }

        match pile.OpenTailOfClosed with
        | Ok p ->
            printf $"debug: Opened: {p.Opened.ToString} Closed: {p.Closed.ToString}"

            Assert.AreEqual(
                { Opened = [| { Suit = Spade; Number = 1 } |]
                  Closed = [||] },
                p
            )
        | _ -> Assert.Fail("Invalid result")

    [<Test>]
    member this.TestOpenTailOfClosedIfPileIsAllClosedAnd2Cards() =
        let pile =
            { Opened = [||]
              Closed = [| { Suit = Spade; Number = 1 }; { Suit = Spade; Number = 2 } |] }

        match pile.OpenTailOfClosed with
        | Ok p ->
            printf $"debug: Opened: {p.Opened.ToString} Closed: {p.Closed.ToString}"

            Assert.AreEqual(
                { Opened = [| { Suit = Spade; Number = 2 } |]
                  Closed = [| { Suit = Spade; Number = 1 } |] },
                p
            )
        | _ -> Assert.Fail("Invalid result")

    [<Test>]
    member this.TestOpenTailOfClosedIfAllOpened() =
        let pile =
            { Opened = [| { Suit = Spade; Number = 1 } |]
              Closed = [||] }

        match pile.OpenTailOfClosed with
        | Error m -> Assert.AreEqual("Opened cards is not Empty", m)
        | _ -> Assert.Fail("Invalid result")

    [<Test>]
    member this.TestOpenTailOfClosedIfOthers() =
        let pile =
            { Opened = [| { Suit = Spade; Number = 1 } |]
              Closed = [| { Suit = Spade; Number = 2 } |] }

        match pile.OpenTailOfClosed with
        | Error m -> Assert.AreEqual("Opened cards is not Empty", m)
        | _ -> Assert.Fail("Invalid result")

    [<Test>]
    member this.TestPickFromOpenedIfOpendCardsIsEmpty() =
        let pile = { Opened = [||]; Closed = [||] }

        match pile.PickFromOpened 1 with
        | Error m -> Assert.AreEqual("Opened cards is Empty in this Pile", m)
        | _ -> Assert.Fail("Invalid result")

    [<Test>]
    member this.TestPickFromOpenedIfCountIsGreaterThanOpenedCount() =
        let pile =
            { Opened = [| { Suit = Spade; Number = 1 } |]
              Closed = [||] }

        match pile.PickFromOpened 2 with
        | Error m -> Assert.AreEqual("Opened cards is Empty in this Pile", m)
        | _ -> Assert.Fail("Invalid result")

    [<Test>]
    member this.TestPickFromOpenedIfAllOpenedAndPick1Card() =
        let pile =
            { Opened = [| { Suit = Spade; Number = 1 } |]
              Closed = [||] }

        match pile.PickFromOpened 1 with
        | Ok (picked, newPile) ->
            Assert.AreEqual([| { Suit = Spade; Number = 1 } |], picked)
            Assert.AreEqual({ Opened = [||]; Closed = [||] }, newPile)
        | _ -> Assert.Fail("Invalid result")

    [<Test>]
    member this.TestPickFromOpenedIfAllOpenedAndPick2Card() =
        let pile =
            { Opened = [| { Suit = Spade; Number = 2 }; { Suit = Spade; Number = 1 } |]
              Closed = [||] }

        match pile.PickFromOpened 1 with
        | Ok (picked, newPile) ->
            Assert.AreEqual([| { Suit = Spade; Number = 1 } |], picked)

            Assert.AreEqual(
                { Opened = [| { Suit = Spade; Number = 2 } |]
                  Closed = [||] },
                newPile
            )

        | _ -> Assert.Fail("Invalid result")

    [<Test>]
    member this.TestPickFromOpenedIfPileHasBoth() =
        let pile =
            { Opened = [| { Suit = Spade; Number = 2 }; { Suit = Spade; Number = 1 } |]
              Closed = [| { Suit = Spade; Number = 3 } |] }

        match pile.PickFromOpened 1 with
        | Ok (picked, newPile) ->
            Assert.AreEqual([| { Suit = Spade; Number = 1 } |], picked)

            Assert.AreEqual(
                { Opened = [| { Suit = Spade; Number = 2 } |]
                  Closed = [| { Suit = Spade; Number = 3 } |] },
                newPile
            )

        | _ -> Assert.Fail("Invalid result")

    [<Test>]
    member this.TestCanPutToOpenedIfTailIsBlackAndCardIsBlack() =
        let pile =
            { Opened = [| { Suit = Spade; Number = 2 } |]
              Closed = [||] }

        Assert.False(pile.CanPutToOpened { Suit = Club; Number = 1 })

    [<Test>]
    member this.TestCanPutToOpenedIfTailIsBlackAndCardIsRedAndBefore() =
        let pile =
            { Opened = [| { Suit = Spade; Number = 2 } |]
              Closed = [||] }

        Assert.True(pile.CanPutToOpened { Suit = Heart; Number = 1 })

    [<Test>]
    member this.TestCanPutToOpenedIfTailIsBlackAndCardIsRedAndNotBefore() =
        let pile =
            { Opened = [| { Suit = Spade; Number = 2 } |]
              Closed = [||] }

        Assert.False(pile.CanPutToOpened { Suit = Heart; Number = 2 })

    [<Test>]
    member this.TestCanPutToOpenedIfTailIsBlackAndCardIsRedAndNext() =
        let pile =
            { Opened = [| { Suit = Spade; Number = 2 } |]
              Closed = [||] }

        Assert.False(pile.CanPutToOpened { Suit = Heart; Number = 3 })

    [<Test>]
    member this.TestPutCardsIfCardsIsEmpty() =
        let pile = { Opened = [||]; Closed = [||] }

        match pile.PutCards [||] with
        | Error m -> Assert.AreEqual("No cards to put", m)
        | _ -> Assert.Fail("Invalid result")

    [<Test>]
    member this.TestPutCardsIfPileIsEmptyAndHeadOfCardsIsKing() =
        let pile = { Opened = [||]; Closed = [||] }

        match
            pile.PutCards
                [| { Suit = Spade; Number = 13 }
                   { Suit = Heart; Number = 12 }
                   { Suit = Club; Number = 11 }
                   { Suit = Diamond; Number = 10 } |]
        with
        | Ok p ->
            Assert.AreEqual(
                { Opened =
                    [| { Suit = Spade; Number = 13 }
                       { Suit = Heart; Number = 12 }
                       { Suit = Club; Number = 11 }
                       { Suit = Diamond; Number = 10 } |]
                  Closed = [||] },
                p
            )
        | _ -> Assert.Fail("Invalid result")

    [<Test>]
    member this.TestPutCardsIfPileIsEmptyAndHeadOfCardsIsNotKing() =
        let pile = { Opened = [||]; Closed = [||] }

        match
            pile.PutCards
                [| { Suit = Heart; Number = 12 }
                   { Suit = Club; Number = 11 }
                   { Suit = Diamond; Number = 10 } |]
        with
        | Error m -> Assert.AreEqual("Cannot put the cards when head number is not 13", m)
        | _ -> Assert.Fail("Invalid result")

    [<Test>]
    member this.TestPutCardsIfPileIsAllClosed() =
        let pile =
            { Opened = [||]
              Closed = [| { Suit = Spade; Number = 1 } |] }

        match
            pile.PutCards
                [| { Suit = Heart; Number = 12 }
                   { Suit = Club; Number = 11 }
                   { Suit = Diamond; Number = 10 } |]
        with
        | Error m -> Assert.AreEqual("Please deal the tail of Closed cards", m)
        | _ -> Assert.Fail("Invalid result")

    [<Test>]
    member this.TestPutCardsIfPileIsOthersAndCanPutOnOpened() =
        let pile =
            { Opened = [| { Suit = Spade; Number = 13 } |]
              Closed = [| { Suit = Spade; Number = 1 } |] }

        match
            pile.PutCards
                [| { Suit = Heart; Number = 12 }
                   { Suit = Club; Number = 11 }
                   { Suit = Diamond; Number = 10 } |]
        with
        | Ok p ->
            Assert.AreEqual(
                { Opened =
                    [| { Suit = Spade; Number = 13 }
                       { Suit = Heart; Number = 12 }
                       { Suit = Club; Number = 11 }
                       { Suit = Diamond; Number = 10 } |]
                  Closed = [| { Suit = Spade; Number = 1 } |] },
                p
            )
        | _ -> Assert.Fail("Invalid result")

    [<Test>]
    member this.TestPutCardsIfPileIsOthersAndCannotPutOnOpened() =
        let pile =
            { Opened = [| { Suit = Diamond; Number = 13 } |]
              Closed = [| { Suit = Spade; Number = 1 } |] }

        match
            pile.PutCards
                [| { Suit = Heart; Number = 12 }
                   { Suit = Club; Number = 11 }
                   { Suit = Diamond; Number = 10 } |]
        with
        | Error m ->
            Assert.AreEqual(
                "Cannot put the cards when the number of the head card or color of the head card is mismatched",
                m
            )
        | _ -> Assert.Fail("Invalid result")
