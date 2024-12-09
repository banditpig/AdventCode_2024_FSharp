module AdventCode_2024_FSharp_Tests

open Arrays
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

    [<Test>]
    member this.``Finds a block of contiguous elements in the middle of the array``() =
        let arr = [| 1; 2; 2; 2; 3; 4 |]
        let result = contiguousBlockOf 2 0 arr
        ClassicAssert.AreEqual(Some(1, 3), result)

    [<Test>]
    member this.``Finds a block starting from a given index``() =
        let arr = [| 1; 1; 2; 2; 3; 4 |]
        let result = contiguousBlockOf 2 2 arr
        ClassicAssert.AreEqual(Some(2, 3), result)

    [<Test>]
    member this.``Returns None if no contiguous block exists``() =
        let arr = [| 1; 2; 3; 4 |]
        let result = contiguousBlockOf 5 0 arr
        ClassicAssert.AreEqual(None, result)

    [<Test>]
    member this.``Handles an empty array gracefully``() =
        let arr = [||]
        let result = contiguousBlockOf 2 0 arr
        ClassicAssert.AreEqual(None, result)

    [<Test>]
    member this.``Returns None if from index is out of bounds``() =
        let arr = [| 1; 2; 3; 4 |]
        let result = contiguousBlockOf 2 10 arr
        ClassicAssert.AreEqual(None, result)

    [<Test>]
    member this.``Handles blocks that extend to the end of the array``() =
        let arr = [| 1; 2; 2; 2 |]
        let result = contiguousBlockOf 2 1 arr
        ClassicAssert.AreEqual(Some(1, 3), result)
