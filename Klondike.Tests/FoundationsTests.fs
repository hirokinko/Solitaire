namespace Klondike.Tests

open NUnit.Framework
open Klondike.Klondike

[<TestFixture>]
type FoundationsTests() =

    [<Test>]
    member this.TestSpadesArrayIsEmptyAndCardIsAce() =
        let foundations =
            { InnerMap = Map [ (Spade, [||]); (Club, [||]); (Heart, [||]); (Diamond, [||]) ] }

        let card = Card(Spade, 1)

        match foundations.PutCard card with
        | Ok f -> Assert.AreEqual([| Card(Spade, 1) |], f.InnerMap[Spade])
        | _ -> Assert.Fail("Invalid result")

    [<Test>]
    member this.TestSpadesArrayIsEmptyAndCardIsNotAce() =
        let foundations =
            { InnerMap = Map [ (Spade, [||]); (Club, [||]); (Heart, [||]); (Diamond, [||]) ] }

        let card = Card(Spade, 2)

        match foundations.PutCard card with
        | Error m -> Assert.AreEqual("Cannot put the card on this Suit", m)
        | _ -> Assert.Fail("Invalid result")

    [<Test>]
    member this.TestOneCardInSpadesArrayAndCardIsNext() =
        let foundations =
            { InnerMap = Map [ (Spade, [| Card(Spade, 1) |]); (Club, [||]); (Heart, [||]); (Diamond, [||]) ] }

        let card = Card(Spade, 2)

        match foundations.PutCard card with
        | Ok f -> Assert.AreEqual([| Card(Spade, 1); Card(Spade, 2) |], f.InnerMap[Spade])
        | _ -> Assert.Fail("Invalid result")

    [<Test>]
    member this.TestOneCardInSpadesArrayAndCardIsNotNext() =
        let foundations =
            { InnerMap = Map [ (Spade, [| Card(Spade, 1) |]); (Club, [||]); (Heart, [||]); (Diamond, [||]) ] }

        let card = Card(Spade, 3)

        match foundations.PutCard card with
        | Error m -> Assert.AreEqual("Cannot put the card on this Suit", m)
        | _ -> Assert.Fail("Invalid result")
