module Solitaire.Tests

open NUnit.Framework
open FsUnit
open Solitaire.Cards

[<TestCase(0, Suit.Spade, 1)>]
[<TestCase(1, Suit.Heart, 1)>]
[<TestCase(2, Suit.Club, 1)>]
[<TestCase(3, Suit.Diamond, 1)>]
[<TestCase(48, Suit.Spade, 13)>]
[<TestCase(49, Suit.Heart, 13)>]
[<TestCase(50, Suit.Club, 13)>]
[<TestCase(51, Suit.Diamond, 13)>]
let toNumberCardTestcase (given, expectedSuit, expectedNumber) =
    toNumberCard given
    |> should equal (NumberCard(suit = expectedSuit, number = expectedNumber))

[<TestCase(Suit.Spade, 1, ColorOfSuit.Black)>]
[<TestCase(Suit.Club, 1, ColorOfSuit.Black)>]
[<TestCase(Suit.Heart, 1, ColorOfSuit.Red)>]
[<TestCase(Suit.Diamond, 1, ColorOfSuit.Red)>]
let colorTests (givenSuit, givenNumber, expected) =
    NumberCard(suit = givenSuit, number = givenNumber).Color
    |> should equal (Some(expected |> enum<ColorOfSuit>))

[<TestCase(Suit.Spade, 1, true)>]
[<TestCase(Suit.Club, 2, false)>]
let isAceTests (givenSuit, givenNumber, expected) =
    NumberCard(suit = givenSuit, number = givenNumber).IsAce
    |> should equal expected

[<TestCase(Suit.Spade, 13, true)>]
[<TestCase(Suit.Club, 12, false)>]
let isKingTests (givenSuit, givenNumber, expected) =
    NumberCard(suit = givenSuit, number = givenNumber).IsKing
    |> should equal expected

let ``Joker.Color should be None`` () =
    Joker.Color |> should equal None

let ``Joker.IsAce should be false`` () =
    Joker.IsAce |> should equal false

let ``Joker.IsKing should be false`` () =
    Joker.IsKing |> should equal false

let createDeckTestWithoutJoker =
    createDeck 0
    |> should
        equivalent
        [ NumberCard(suit = Suit.Spade, number = 1)
          NumberCard(suit = Suit.Heart, number = 1)
          NumberCard(suit = Suit.Club, number = 1)
          NumberCard(suit = Suit.Diamond, number = 1)
          NumberCard(suit = Suit.Spade, number = 2)
          NumberCard(suit = Suit.Heart, number = 2)
          NumberCard(suit = Suit.Club, number = 2)
          NumberCard(suit = Suit.Diamond, number = 2)
          NumberCard(suit = Suit.Spade, number = 3)
          NumberCard(suit = Suit.Heart, number = 3)
          NumberCard(suit = Suit.Club, number = 3)
          NumberCard(suit = Suit.Diamond, number = 3)
          NumberCard(suit = Suit.Spade, number = 4)
          NumberCard(suit = Suit.Heart, number = 4)
          NumberCard(suit = Suit.Club, number = 4)
          NumberCard(suit = Suit.Diamond, number = 4)
          NumberCard(suit = Suit.Spade, number = 5)
          NumberCard(suit = Suit.Heart, number = 5)
          NumberCard(suit = Suit.Club, number = 5)
          NumberCard(suit = Suit.Diamond, number = 5)
          NumberCard(suit = Suit.Spade, number = 6)
          NumberCard(suit = Suit.Heart, number = 6)
          NumberCard(suit = Suit.Club, number = 6)
          NumberCard(suit = Suit.Diamond, number = 6)
          NumberCard(suit = Suit.Spade, number = 7)
          NumberCard(suit = Suit.Heart, number = 7)
          NumberCard(suit = Suit.Club, number = 7)
          NumberCard(suit = Suit.Diamond, number = 7)
          NumberCard(suit = Suit.Spade, number = 8)
          NumberCard(suit = Suit.Heart, number = 8)
          NumberCard(suit = Suit.Club, number = 8)
          NumberCard(suit = Suit.Diamond, number = 8)
          NumberCard(suit = Suit.Spade, number = 9)
          NumberCard(suit = Suit.Heart, number = 9)
          NumberCard(suit = Suit.Club, number = 9)
          NumberCard(suit = Suit.Diamond, number = 9)
          NumberCard(suit = Suit.Spade, number = 10)
          NumberCard(suit = Suit.Heart, number = 10)
          NumberCard(suit = Suit.Club, number = 10)
          NumberCard(suit = Suit.Diamond, number = 10)
          NumberCard(suit = Suit.Spade, number = 11)
          NumberCard(suit = Suit.Heart, number = 11)
          NumberCard(suit = Suit.Club, number = 11)
          NumberCard(suit = Suit.Diamond, number = 11)
          NumberCard(suit = Suit.Spade, number = 12)
          NumberCard(suit = Suit.Heart, number = 12)
          NumberCard(suit = Suit.Club, number = 12)
          NumberCard(suit = Suit.Diamond, number = 12)
          NumberCard(suit = Suit.Spade, number = 13)
          NumberCard(suit = Suit.Heart, number = 13)
          NumberCard(suit = Suit.Club, number = 13)
          NumberCard(suit = Suit.Diamond, number = 13)
          ]

let createDeckTestJoker =
    createDeck 1
    |> should
        equivalent
        [ NumberCard(suit = Suit.Spade, number = 1)
          NumberCard(suit = Suit.Heart, number = 1)
          NumberCard(suit = Suit.Club, number = 1)
          NumberCard(suit = Suit.Diamond, number = 1)
          NumberCard(suit = Suit.Spade, number = 2)
          NumberCard(suit = Suit.Heart, number = 2)
          NumberCard(suit = Suit.Club, number = 2)
          NumberCard(suit = Suit.Diamond, number = 2)
          NumberCard(suit = Suit.Spade, number = 3)
          NumberCard(suit = Suit.Heart, number = 3)
          NumberCard(suit = Suit.Club, number = 3)
          NumberCard(suit = Suit.Diamond, number = 3)
          NumberCard(suit = Suit.Spade, number = 4)
          NumberCard(suit = Suit.Heart, number = 4)
          NumberCard(suit = Suit.Club, number = 4)
          NumberCard(suit = Suit.Diamond, number = 4)
          NumberCard(suit = Suit.Spade, number = 5)
          NumberCard(suit = Suit.Heart, number = 5)
          NumberCard(suit = Suit.Club, number = 5)
          NumberCard(suit = Suit.Diamond, number = 5)
          NumberCard(suit = Suit.Spade, number = 6)
          NumberCard(suit = Suit.Heart, number = 6)
          NumberCard(suit = Suit.Club, number = 6)
          NumberCard(suit = Suit.Diamond, number = 6)
          NumberCard(suit = Suit.Spade, number = 7)
          NumberCard(suit = Suit.Heart, number = 7)
          NumberCard(suit = Suit.Club, number = 7)
          NumberCard(suit = Suit.Diamond, number = 7)
          NumberCard(suit = Suit.Spade, number = 8)
          NumberCard(suit = Suit.Heart, number = 8)
          NumberCard(suit = Suit.Club, number = 8)
          NumberCard(suit = Suit.Diamond, number = 8)
          NumberCard(suit = Suit.Spade, number = 9)
          NumberCard(suit = Suit.Heart, number = 9)
          NumberCard(suit = Suit.Club, number = 9)
          NumberCard(suit = Suit.Diamond, number = 9)
          NumberCard(suit = Suit.Spade, number = 10)
          NumberCard(suit = Suit.Heart, number = 10)
          NumberCard(suit = Suit.Club, number = 10)
          NumberCard(suit = Suit.Diamond, number = 10)
          NumberCard(suit = Suit.Spade, number = 11)
          NumberCard(suit = Suit.Heart, number = 11)
          NumberCard(suit = Suit.Club, number = 11)
          NumberCard(suit = Suit.Diamond, number = 11)
          NumberCard(suit = Suit.Spade, number = 12)
          NumberCard(suit = Suit.Heart, number = 12)
          NumberCard(suit = Suit.Club, number = 12)
          NumberCard(suit = Suit.Diamond, number = 12)
          NumberCard(suit = Suit.Spade, number = 13)
          NumberCard(suit = Suit.Heart, number = 13)
          NumberCard(suit = Suit.Club, number = 13)
          NumberCard(suit = Suit.Diamond, number = 13)
          Joker
          ]
