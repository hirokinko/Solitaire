namespace Klondike.Tests

open NUnit.Framework
open Klondike.Klondike

[<TestFixture>]
type PileStatusTests() =

    [<Test>]
    member this.TestStatusIsEmpty() =
        Assert.AreEqual(Empty, { Opened = [||]; Closed = [||] }.Status)

    [<Test>]
    member this.TestStatusIsAllClosed() =
        Assert.AreEqual(
            AllClosed,
            { Opened = [||]
              Closed = [| Card(Spade, 1) |] }
                .Status
        )

    [<Test>]
    member this.TestStatusIsAllOpened() =
        Assert.AreEqual(
            AllOpened,
            { Opened = [| Card(Spade, 1) |]
              Closed = [||] }
                .Status
        )

    [<Test>]
    member this.TestStatusIsOthers() =
        Assert.AreEqual(
            Others,
            { Opened = [| Card(Spade, 1) |]
              Closed = [| Card(Spade, 2) |] }
                .Status
        )

[<TestFixture>]
type PileOpenTailOfClosedTests() =

    [<Test>]
    member this.TestIfPileIsEmpty() =
        let pile = { Opened = [||]; Closed = [||] }

        match pile.OpenTailOfClosed with
        | Error m -> Assert.AreEqual("This pile is Empty", m)
        | _ -> Assert.Fail("Invalid result")

    [<Test>]
    member this.TestIfPileIsAllClosed() =
        let pile =
            { Opened = [||]
              Closed = [| Card(Spade, 1) |] }

        match pile.OpenTailOfClosed with
        | Ok p ->
            printf $"debug: Opened: {p.Opened.ToString} Closed: {p.Closed.ToString}"

            Assert.AreEqual(
                { Opened = [| Card(Spade, 1) |]
                  Closed = [||] },
                p
            )
        | _ -> Assert.Fail("Invalid result")

    [<Test>]
    member this.TestIfPileIsAllClosedAnd2Cards() =
        let pile =
            { Opened = [||]
              Closed = [| Card(Spade, 1); Card(Spade, 2) |] }

        match pile.OpenTailOfClosed with
        | Ok p ->
            printf $"debug: Opened: {p.Opened.ToString} Closed: {p.Closed.ToString}"

            Assert.AreEqual(
                { Opened = [| Card(Spade, 2) |]
                  Closed = [| Card(Spade, 1) |] },
                p
            )
        | _ -> Assert.Fail("Invalid result")

    [<Test>]
    member this.TestIfPileIsAllOpened() =
        let pile =
            { Opened = [| Card(Spade, 1) |]
              Closed = [||] }

        match pile.OpenTailOfClosed with
        | Error m -> Assert.AreEqual("Opened cards is not Empty", m)
        | _ -> Assert.Fail("Invalid result")

    [<Test>]
    member this.TestIfPileIsOthers() =
        let pile =
            { Opened = [| Card(Spade, 1) |]
              Closed = [| Card(Spade, 2) |] }

        match pile.OpenTailOfClosed with
        | Error m -> Assert.AreEqual("Opened cards is not Empty", m)
        | _ -> Assert.Fail("Invalid result")

[<TestFixture>]
type PickFromOpenedTests() =

    [<Test>]
    member this.TestIfPileIsOpendCardsIsEmpty() =
        let pile = { Opened = [||]; Closed = [||] }

        match pile.PickFromOpened 1 with
        | Error m -> Assert.AreEqual("Opened cards is Empty in this Pile", m)
        | _ -> Assert.Fail("Invalid result")

    [<Test>]
    member this.TestIfCountIsGreaterThanOpenedLength() =
        let pile =
            { Opened = [| Card(Spade, 1) |]
              Closed = [||] }

        match pile.PickFromOpened 2 with
        | Error m -> Assert.AreEqual("Opened cards is Empty in this Pile", m)
        | _ -> Assert.Fail("Invalid result")

    [<Test>]
    member this.TestIfPileIsAllOpenedAndPick1Card() =
        let pile =
            { Opened = [| Card(Spade, 1) |]
              Closed = [||] }

        match pile.PickFromOpened 1 with
        | Ok (picked, newPile) ->
            Assert.AreEqual([| Card(Spade, 1) |], picked)
            Assert.AreEqual({ Opened = [||]; Closed = [||] }, newPile)
        | _ -> Assert.Fail("Invalid result")

    [<Test>]
    member this.TestIfPileIsAllOpenedAndPick2Card() =
        let pile =
            { Opened = [| Card(Spade, 2); Card(Spade, 1) |]
              Closed = [||] }

        match pile.PickFromOpened 1 with
        | Ok (picked, newPile) ->
            Assert.AreEqual([| Card(Spade, 1) |], picked)

            Assert.AreEqual(
                { Opened = [| Card(Spade, 2) |]
                  Closed = [||] },
                newPile
            )

        | _ -> Assert.Fail("Invalid result")

    [<Test>]
    member this.TestIfPileHasBoth() =
        let pile =
            { Opened = [| Card(Spade, 2); Card(Spade, 1) |]
              Closed = [| Card(Spade, 3) |] }

        match pile.PickFromOpened 1 with
        | Ok (picked, newPile) ->
            Assert.AreEqual([| Card(Spade, 1) |], picked)

            Assert.AreEqual(
                { Opened = [| Card(Spade, 2) |]
                  Closed = [| Card(Spade, 3) |] },
                newPile
            )

        | _ -> Assert.Fail("Invalid result")

[<TestFixture>]
type CanPutToOpenedTests() =

    [<Test>]
    member this.TestIfTailIsBlackAndCardIsBlack() =
        let pile =
            { Opened = [| Card(Spade, 2) |]
              Closed = [||] }

        Card(Club, 1) |> pile.CanPutToOpened |> Assert.False

    [<Test>]
    member this.TestIfTailIsBlackAndCardIsRedAndBefore() =
        let pile =
            { Opened = [| Card(Spade, 2) |]
              Closed = [||] }

        Card(Heart, 1) |> pile.CanPutToOpened |> Assert.True

    [<Test>]
    member this.TestIfTailIsBlackAndCardIsRedAndNotBefore() =
        let pile =
            { Opened = [| Card(Spade, 2) |]
              Closed = [||] }

        Card(Heart, 2) |> pile.CanPutToOpened |> Assert.False

    [<Test>]
    member this.TestIfTailIsBlackAndCardIsRedAndNext() =
        let pile =
            { Opened = [| Card(Spade, 2) |]
              Closed = [||] }

        Card(Heart, 3) |> pile.CanPutToOpened |> Assert.False

[<TestFixture>]
type PutCardsTests() =

    [<Test>]
    member this.TestIfCardsAreEmpty() =
        let pile = { Opened = [||]; Closed = [||] }

        match pile.PutCards [||] with
        | Error m -> Assert.AreEqual("No cards to put", m)
        | _ -> Assert.Fail("Invalid result")

    [<Test>]
    member this.TestIfPileIsEmptyAndHeadOfCardsIsKing() =
        let pile = { Opened = [||]; Closed = [||] }

        match pile.PutCards [| Card(Spade, 13); Card(Heart, 12); Card(Club, 11); Card(Diamond, 10) |] with
        | Ok p ->
            Assert.AreEqual(
                { Opened = [| Card(Spade, 13); Card(Heart, 12); Card(Club, 11); Card(Diamond, 10) |]
                  Closed = [||] },
                p
            )
        | _ -> Assert.Fail("Invalid result")

    [<Test>]
    member this.TestIfPileIsEmptyAndHeadOfCardsIsNotKing() =
        let pile = { Opened = [||]; Closed = [||] }

        match pile.PutCards [| Card(Heart, 12); Card(Club, 11); Card(Diamond, 10) |] with
        | Error m -> Assert.AreEqual("Cannot put the cards when head number is not 13", m)
        | _ -> Assert.Fail("Invalid result")

    [<Test>]
    member this.TestIfPileIsAllClosed() =
        let pile =
            { Opened = [||]
              Closed = [| Card(Spade, 1) |] }

        match pile.PutCards [| Card(Heart, 12); Card(Club, 11); Card(Diamond, 10) |] with
        | Error m -> Assert.AreEqual("Please deal the tail of Closed cards", m)
        | _ -> Assert.Fail("Invalid result")

    [<Test>]
    member this.TestIfPileIsOthersAndCanPutOnOpened() =
        let pile =
            { Opened = [| Card(Spade, 13) |]
              Closed = [| Card(Spade, 1) |] }

        match pile.PutCards [| Card(Heart, 12); Card(Club, 11); Card(Diamond, 10) |] with
        | Ok p ->
            Assert.AreEqual(
                { Opened = [| Card(Spade, 13); Card(Heart, 12); Card(Club, 11); Card(Diamond, 10) |]
                  Closed = [| Card(Spade, 1) |] },
                p
            )
        | _ -> Assert.Fail("Invalid result")

    [<Test>]
    member this.TestPutCardsIfPileIsOthersAndCannotPutOnOpened() =
        let pile =
            { Opened = [| Card(Diamond, 13) |]
              Closed = [| Card(Spade, 1) |] }

        match pile.PutCards [| Card(Heart, 12); Card(Club, 11); Card(Diamond, 10) |] with
        | Error m ->
            Assert.AreEqual(
                "Cannot put the cards when the number of the head card or color of the head card is mismatched",
                m
            )
        | _ -> Assert.Fail("Invalid result")
