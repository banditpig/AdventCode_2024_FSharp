module AdventCode_2024_FSharp_Tests

open NUnit.Framework
open NUnit.Framework.Legacy
open Day8

[<TestFixture>]
type DistanceTests() =

    [<Test>]
    member this.``Distance between same points is 0``() =
        ClassicAssert.AreEqual(0, distance (1, 1) (1, 1))

    [<Test>]
    member this.``Distance between adjacent points is 1``() =
        ClassicAssert.AreEqual(1, distance (1, 1) (1, 2))
        ClassicAssert.AreEqual(1, distance (1, 1) (2, 1))

    [<Test>]
    member this.``Distance between points on same row is correct``() =
        ClassicAssert.AreEqual(3, distance (1, 1) (1, 4))

    [<Test>]
    member this.``Distance between points on same column is correct``() =
        ClassicAssert.AreEqual(3, distance (1, 1) (4, 1))

    [<Test>]
    member this.``Distance between points in different quadrants is correct``() =
        ClassicAssert.AreEqual(11, distance (-2, -2) (3, 4))
[<TestFixture>]
type PointsOnLineWithinGridTests() =

    [<Test>]
    member this.``Points on horizontal line``() =
        let points = pointsOnLineWithinGrid (0, 0) (3, 0) 
        ClassicAssert.AreEqual([(0, 0); (1, 0); (2, 0); (3, 0)], points)

    [<Test>]
    member this.``Points on vertical line``() =
        let points = pointsOnLineWithinGrid (0, 0) (0, 3) (5, 5)
        ClassicAssert.AreEqual([(0, 0); (0, 1); (0, 2); (0, 3)], points)

    [<Test>]
    member this.``Points on diagonal line``() =
        let points = pointsOnLineWithinGrid (0, 0) (3, 3) (5, 5)
        ClassicAssert.AreEqual([(0, 0); (1, 1); (2, 2); (3, 3)], points)

    [<Test>]
    member this.``Points on line outside grid``() =
        let points = pointsOnLineWithinGrid (0, 0) (6, 6) (5, 5)
        ClassicAssert.AreEqual([(0, 0); (1, 1); (2, 2); (3, 3); (4, 4); (5, 5)], points)

    [<Test>]
    member this.``Points on line with negative coordinates``() =
        let points = pointsOnLineWithinGrid (-2, -2) (2, 2) (5, 5)
        ClassicAssert.AreEqual([(-2, -2); (-1, -1); (0, 0); (1, 1); (2, 2)], points)