namespace Klondike.Tests

open NUnit.Framework
open Klondike.Klondike

[<TestFixture>]
type PropertiesTests() =

    [<Test>]
    member this.TestIsKing() = Card(Spade, 13).IsKing |> Assert.True

    [<Test>]
    member this.TestIsNotKing() = Card(Spade, 12).IsKing |> Assert.False

    [<Test>]
    member this.TestIsAce() = Card(Spade, 1).IsAce |> Assert.True

    [<Test>]
    member this.TestIsNotAce() = Card(Spade, 2).IsAce |> Assert.False

    [<Test>]
    member this.TestColorOfSpadeIsBlack() =
        let card = Card(Spade, 1)
        Assert.AreEqual(Black, card.Color)

    [<Test>]
    member this.TestColorOfClubIsBlack() =
        let card = Card(Club, 1)
        Assert.AreEqual(Black, card.Color)

    [<Test>]
    member this.TestColorOfHeartIsRed() =
        let card = Card(Heart, 1)
        Assert.AreEqual(Red, card.Color)

    [<Test>]
    member this.TestColorOfDiamondIsRed() =
        let card = Card(Diamond, 1)
        Assert.AreEqual(Red, card.Color)

    [<Test>]
    member this.Test2IsNextTo1() =
        let card = Card(Spade, 2)
        Card(Spade, 1) |> card.IsNextTo |> Assert.True

    [<Test>]
    member this.Test2IsNotNextTo2() =
        let card = Card(Spade, 2)
        Card(Spade, 2) |> card.IsNextTo |> Assert.False

    [<Test>]
    member this.Test2IsNotNextTo3() =
        let card = Card(Spade, 2)
        Card(Spade, 3) |> card.IsNextTo |> Assert.False

    [<Test>]
    member this.TestIsNotNextToWhenItIsNotSequential() =
        let card = Card(Spade, 3)
        Card(Spade, 1) |> card.IsNextTo |> Assert.False

    [<Test>]
    member this.Test1IsBefore2() =
        let card = Card(Spade, 1)
        Card(Spade, 2) |> card.IsBefore |> Assert.True

    [<Test>]
    member this.Test1IsNotBefore1() =
        let card = Card(Spade, 1)
        Card(Spade, 1) |> card.IsBefore |> Assert.False

    [<Test>]
    member this.Test2IsNotBefore1() =
        let card = Card(Spade, 2)
        Card(Spade, 1) |> card.IsBefore |> Assert.False

    [<Test>]
    member this.TestIsNotBeforeWhenItIsNotSequential() =
        let card = Card(Spade, 1)
        Card(Spade, 1) |> card.IsNextTo |> Assert.False

[<TestFixture>]
type ToCardTests() =
    // TODO: 0 〜 51 までの各値のテストを追加
    [<Test>]
    member this.TestToCard() =
        Card(Spade, 1) = toCard 0 |> Assert.True
        Card(Club, 1) = toCard 1 |> Assert.True
        Card(Heart, 1) = toCard 2 |> Assert.True
        Card(Diamond, 1) = toCard 3 |> Assert.True
        Card(Spade, 13) = toCard 48 |> Assert.True
        Card(Club, 13) = toCard 49 |> Assert.True
        Card(Heart, 13) = toCard 50 |> Assert.True
        Card(Diamond, 13) = toCard 51 |> Assert.True
