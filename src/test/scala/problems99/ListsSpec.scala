package org.laplacetec.study
package problems99

import org.scalatest.funspec.AnyFunSpec

class ListsSpec extends AnyFunSpec {
  val newValue = {
    describe("the solution to P01") {
      it("should return None when list is empty.") {
        assert(Lists.last(List()).isEmpty)
      }

      it("should return the last element.") {
        val toTest = List(0, 1, 2, 3)
        assert(Lists.last(toTest).get == 3)
      }

      it("should work for boolean type too.") {
        val toTest = List(false, false, true)
        assert(Lists.last(toTest).get)
      }
    }

    describe("the solution to P02") {
      it("should return None when list is empty.") {
        assert(Lists.penultimate(List()).isEmpty)
      }

      it("should return None when list has only one element.") {
        assert(Lists.penultimate(List(0)).isEmpty)
      }

      it("should return the first element when the list has two elements.") {
        assert(Lists.penultimate(List(0, 1)).get == 0)
      }

      it("should return the truly penultimate element.") {
        assert(Lists.penultimate(List(0, 1, 2, 3)).get == 2)
      }
    }

    describe("the solution to P03") {
      it("should return None if index >= list len") {
        assert(Lists.nth(4, List(0, 1, 2, 3)).isEmpty)
      }

      it("should return None if index < 0") {
        assert(Lists.nth(-1, List(0, 1, 2, 3)).isEmpty)
      }

      it("should return nth element properly") {
        List(0, 1, 2, 3) foreach (index => {
          assert(Lists.nth(index, List(0, 1, 2, 3)).get == index)
        })
      }
    }

    describe("the solution to flatten") {
      it("should return flatten List") {
        assert(Lists.flatten(List()) == List())
        assert(Lists.flatten(List(1)) == List(1))
        assert(Lists.flatten(List(0, 1, 2, 3)) == List(0, 1, 2, 3))
        assert(Lists.flatten(List(List(0, 1), 2, 3)) == List(0, 1, 2, 3))
        assert(
          Lists.flatten(List(1, 2, 3, List(4, 5, List(6)), List())) == List(1,
            2, 3, 4, 5, 6)
        )
      }
    }

    describe("the solution to compress") {
      it("should return compressed List") {
        assert(Lists.compress[Int](List()) == List())
        assert(Lists.compress[Int](List(1)) == List(1))
        assert(
          Lists.compress[Int](List(0, 0, 0, 0, 1, 2, 3)) == List(0, 1, 2, 3)
        )
        assert(
          Lists
            .compress[Int](List(0, 0, 0, 0, 1, 2, 2, 2, 3)) == List(0, 1, 2, 3)
        )
        assert(Lists.compress[Int](List(0)) == List(0))
        assert(
          Lists.compress[List[Int]](
            List(List(1, 2, 3), List(1, 2, 3), List(4, 5, 6))
          ) == List(List(1, 2, 3), List(4, 5, 6))
        )
      }
    }
    describe("the solution to compress") {
      it("should return pack List") {
        assert(Lists.pack(List()) == Nil)
        assert(Lists.pack(List(1)) == List(List(1)))
        assert(Lists.pack(List(1, 2)) == List(List(1), List(2)))
        assert(
          Lists.pack(
            List(
              "a",
              "a",
              "a",
              "a",
              "b",
              "c",
              "c",
              "a",
              "a",
              "d",
              "e",
              "e",
              "e",
              "e"
            )
          ) == List(
            List("a", "a", "a", "a"),
            List("b"),
            List("c", "c"),
            List("a", "a"),
            List("d"),
            List("e", "e", "e", "e")
          )
        )
      }
    }

    describe("the solution to encode") {
      it("should return encode List") {
        assert(Lists.encode(List()) == Nil)
        assert(Lists.encode(List(1)) == List((1, 1)))
        assert(Lists.encode(List(1, 2)) == List((1, 1), (1, 2)))
        assert(
          Lists.encode(
            List(
              "a",
              "a",
              "a",
              "a",
              "b",
              "c",
              "c",
              "a",
              "a",
              "d",
              "e",
              "e",
              "e",
              "e"
            )
          ) == List((4, "a"), (1, "b"), (2, "c"), (2, "a"), (1, "d"), (4, "e"))
        )
      }
    }

    describe("the solution to encodeModified") {
      it("should return encodeModified List") {
        assert(Lists.encodeModified(List()) == Nil)
        assert(Lists.encodeModified(List(1)) == List(Left(1)))
        assert(Lists.encodeModified(List(1, 2)) == List(Left(1), Left(2)))
        assert(
          Lists.encodeModified(
            List(
              "a",
              "a",
              "a",
              "a",
              "b",
              "c",
              "c",
              "a",
              "a",
              "d",
              "e",
              "e",
              "e",
              "e"
            )
          )
            == List(
              Right((4, "a")),
              Left("b"),
              Right((2, "c")),
              Right((2, "a")),
              Left("d"),
              Right((4, "e"))
            )
        )
      }
    }

    describe("the solution to decode") {
      it("should return decoded List") {
        assert(
          Lists.decode(
            List((4, "a"), (1, "b"), (2, "c"), (2, "a"), (1, "d"), (4, "e"))
          )
            == List(
              "a",
              "a",
              "a",
              "a",
              "b",
              "c",
              "c",
              "a",
              "a",
              "d",
              "e",
              "e",
              "e",
              "e"
            )
        )
        assert(Lists.decode(List()) == Nil)
        assert(Lists.decode(List((1, "b"))) == List("b"))
        assert(Lists.decode(List((1, 2), (2, 3))) == List(2, 3, 3))
        assert(
          Lists.decode(
            List(
              (2, List("a", "a", "a")),
              (3, "BC"),
              (1, List("a", 3))
            )
          ) == List(
            List("a", "a", "a"),
            List("a", "a", "a"),
            "BC",
            "BC",
            "BC",
            List("a", 3)
          )
        )
      }
    }

    describe("the solution to encodeDirectEncode") {

      it("should return decoded List") {
        assert(Lists.encodeDirect(List()) == Nil)
        assert(Lists.encodeDirect(List(1)) == List((1, 1)))
        assert(Lists.encodeDirect(List(1, 2)) == List((1, 1), (1, 2)))
        assert(
          Lists.encodeDirect(
            List(
              "a",
              "a",
              "a",
              "a",
              "b",
              "c",
              "c",
              "a",
              "a",
              "d",
              "e",
              "e",
              "e",
              "e"
            )
          ) == List((4, "a"), (1, "b"), (2, "c"), (2, "a"), (1, "d"), (4, "e"))
        )
      }
    }

    describe("the solution to duplicate") {
      it("should return duplicated list") {
        assert(
          Lists.duplicate(List("a", "b", "c", "c", "d")) == List(
            "a",
            "a",
            "b",
            "b",
            "c",
            "c",
            "c",
            "c",
            "d",
            "d"
          )
        )
      }
    }

    describe("the solution to duplicateNHelper") {
      it("should return duplicated list") {

        assert(
          Lists.duplicateN(3, List("a", "b", "c", "c", "d"))
            == List(
              "a",
              "a",
              "a",
              "b",
              "b",
              "b",
              "c",
              "c",
              "c",
              "c",
              "c",
              "c",
              "d",
              "d",
              "d"
            )
        )
      }
    }

    describe("the solution to drop") {
      it("should return duplicated list") {

        assert(
          Lists.drop(
            3,
            List("a", "b", "c", "d", "e", "f", "g", "h", "i", "j", "k")
          )
            == List("a", "b", "d", "e", "g", "h", "j", "k")
        )
      }
    }

    describe("the solution to drop") {
      it("should return drop list") {
        assert(
          Lists.split(
            3,
            List("a", "b", "c", "d", "e", "f", "g", "h", "i", "j", "k")
          )
            == (List("a", "b", "c"), List(
              "d",
              "e",
              "f",
              "g",
              "h",
              "i",
              "j",
              "k"
            ))
        )
      }
    }

    describe("the solution to split") {
      it("should return slice list") {
        assert(
          Lists.slice(
            3,
            7,
            List("a", "b", "c", "d", "e", "f", "g", "h", "i", "j", "k")
          )
            == List("d", "e", "f", "g")
        )
      }
    }

    // describe("the solution to rotateN") {
    //   it("should return rotateN list") {
    //     assert(
    //       Lists.rotateN(3, List("a", "b", "c", "d", "e", "f", "g", "h", "i", "j", "k"))
    //       == List("d", "e", "f", "g", "h", "i", "j", "k", "a", "b", "c")
    //     )

    //     assert(
    //       Lists.rotateN(-2, List("a", "b", "c", "d", "e", "f", "g", "h", "i", "j", "k"))
    //       == List("j", "k", "a", "b", "c", "d", "e", "f", "g", "h", "i")
    //     )
    //   }
    // }

    describe("the solution to removeKth") {
      it("should return removeKth list") {
        assert(
          Lists.removeAt(1, List("a", "b", "c", "d"))
            == (List("a", "c", "d"), "b")
        )
      }
    }

    describe("the solution to insertAt") {
      it("should return insertAt list") {
        assert(
          Lists.insertAt("new", 1, List("a", "b", "c", "d"))
            == List("a", "new", "b", "c", "d")
        )
      }
    }

    describe { "the solution to combination" } {
      it("should return combination list") {
        assert(
          Lists.combinations(3, List("a", "b", "c", "d", "e", "f"))
            == List(
              List("a", "b", "c"),
              List("a", "b", "d"),
              List("a", "b", "e"),
              List("a", "b", "f"),
              List("a", "c", "d"),
              List("a", "c", "e"),
              List("a", "c", "f"),
              List("a", "d", "e"),
              List("a", "d", "f"),
              List("a", "e", "f"),
              List("b", "c", "d"),
              List("b", "c", "e"),
              List("b", "c", "f"),
              List("b", "d", "e"),
              List("b", "d", "f"),
              List("b", "e", "f"),
              List("c", "d", "e"),
              List("c", "d", "f"),
              List("c", "e", "f"),
              List("d", "e", "f")
            )
        )
        assert(
          Lists.combinations(2, List("a", "b", "c", "d", "e", "f"))
            == List(
              List("a", "b"),
              List("a", "c"),
              List("a", "d"),
              List("a", "e"),
              List("a", "f"),
              List("b", "c"),
              List("b", "d"),
              List("b", "e"),
              List("b", "f"),
              List("c", "d"),
              List("c", "e"),
              List("c", "f"),
              List("d", "e"),
              List("d", "f"),
              List("e", "f")
            )
        )
      }
    }

    describe("Tree") {
      it("should y position Tree") {
        assert(
          Node[Int](
            1,
            Node[Int](
              2,
              Node[Int](
                3,
                Node[Int](4),
                Node[Int](
                  5,
                  Node[Int](6),
                  Node[Int](7)
                )
              ),
              Node[Int](8)
            ),
            Node[Int](
              9,
              Node[Int](
                10,
                End,
                Node[Int](11)
              ),
              End
            )
          ).layoutBinaryTree2 ==
            PositionedNode[Int](
              1,
              PositionedNode[Int](
                2,
                PositionedNode[Int](
                  3,
                  PositionedNode[Int](4, End, End, 1, 4),
                  PositionedNode[Int](
                    5,
                    PositionedNode[Int](6, End, End, 4, 5),
                    PositionedNode[Int](7, End, End, 6, 5),
                    5,
                    4
                  ),
                  3,
                  3
                ),
                PositionedNode[Int](8, End, End, 11, 3),
                7,
                2
              ),
              PositionedNode[Int](
                9,
                PositionedNode[Int](
                  10,
                  End,
                  PositionedNode[Int](11, End, End, 21, 4),
                  19,
                  3
                ),
                End,
                23,
                2
              ),
              15,
              1
            )
        )
      }
    }
  }
}
