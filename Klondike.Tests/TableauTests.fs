namespace Klondike.Tests

open NUnit.Framework
open Klondike.Klondike

[<TestFixture>]
type TableauInitTests() =

    [<Test>]
    member this.TestOk() =
        let cards = [| for i in 0..28 -> toCard i |]

        let expectedTableau =
            { Piles =
                [| { Opened = [| Card(Spade, 1) |] // Pile 1
                     Closed = [||] }

                   { Opened = [| Card(Heart, 1) |]
                     Closed = [| Card(Club, 1) |] } // Pile 2

                   { Opened = [| Card(Club, 2) |]
                     Closed = [| Card(Diamond, 1); Card(Spade, 2) |] } // Pile 3

                   { Opened = [| Card(Club, 3) |]
                     Closed = [| Card(Heart, 2); Card(Diamond, 2); Card(Spade, 3) |] } // Pile 4

                   { Opened = [| Card(Heart, 4) |]
                     Closed = [| Card(Heart, 3); Card(Diamond, 3); Card(Spade, 4); Card(Club, 4) |] } // Pile 5

                   { Opened = [| Card(Spade, 6) |]
                     Closed =
                       [| Card(Diamond, 4)
                          Card(Spade, 5)
                          Card(Club, 5)
                          Card(Heart, 5)
                          Card(Diamond, 5) |] } // Pile 6

                   { Opened = [| Card(Diamond, 7) |]
                     Closed =
                       [| Card(Club, 6)
                          Card(Heart, 6)
                          Card(Diamond, 6)
                          Card(Spade, 7)
                          Card(Club, 7)
                          Card(Heart, 7) |] } |] } // Pile 7

        let expectedStock = [| Card(Spade, 8) |]

        match Tableau.Init cards with
        | Ok result ->
            let actualTableau, actualStock = result

            Assert.AreEqual(expectedTableau, actualTableau)
            Assert.AreEqual(expectedStock, actualStock)

        | Error _ -> Assert.Fail("Invalid result")

    [<Test>]
    member this.TestOkIfCountOfCardsIsExactly() =
        let cards = [| for i in 0..27 -> toCard i |]

        let expectedTableau =
            { Piles =
                [| { Opened = [| Card(Spade, 1) |] // Pile 1
                     Closed = [||] }

                   { Opened = [| Card(Heart, 1) |]
                     Closed = [| Card(Club, 1) |] } // Pile 2

                   { Opened = [| Card(Club, 2) |]
                     Closed = [| Card(Diamond, 1); Card(Spade, 2) |] } // Pile 3

                   { Opened = [| Card(Club, 3) |]
                     Closed = [| Card(Heart, 2); Card(Diamond, 2); Card(Spade, 3) |] } // Pile 4

                   { Opened = [| Card(Heart, 4) |]
                     Closed = [| Card(Heart, 3); Card(Diamond, 3); Card(Spade, 4); Card(Club, 4) |] } // Pile 5

                   { Opened = [| Card(Spade, 6) |]
                     Closed =
                       [| Card(Diamond, 4)
                          Card(Spade, 5)
                          Card(Club, 5)
                          Card(Heart, 5)
                          Card(Diamond, 5) |] } // Pile 6

                   { Opened = [| Card(Diamond, 7) |]
                     Closed =
                       [| Card(Club, 6)
                          Card(Heart, 6)
                          Card(Diamond, 6)
                          Card(Spade, 7)
                          Card(Club, 7)
                          Card(Heart, 7) |] } |] } // Pile 7

        let expectedStock = [||]

        match Tableau.Init cards with
        | Ok result ->
            let actualTableau, actualStock = result

            Assert.AreEqual(expectedTableau, actualTableau)
            Assert.AreEqual(expectedStock, actualStock)

        | Error _ -> Assert.Fail("Invalid result")

    [<Test>]
    member this.TestError() =
        let cards = [| for i in 0..26 -> toCard i |]

        match Tableau.Init cards with
        | Ok _ -> Assert.Fail("Invalid result")
        | Error m -> Assert.AreEqual("Not enough cards", m)
