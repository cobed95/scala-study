package org.laplacetec.study
package problems99

import org.scalatest.funspec.AnyFunSpec

class ListsSpec extends AnyFunSpec{
  describe("In the lists section of the 99 Problems in Scala,") {
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
        assert(Lists.flatten(List(1,2,3, List(4,5,List(6)), List())) == List(1,2,3,4,5,6))
      }
    }

    describe("the solution to compress") {
      it("should return compressed List") {
        assert(Lists.compress[Int](List()) == List())
        assert(Lists.compress[Int](List(1)) == List(1))
        assert(Lists.compress[Int](List(0, 0, 0, 0, 1, 2, 3)) == List(0, 1, 2, 3))
        assert(Lists.compress[Int](List(0, 0, 0, 0, 1, 2, 2, 2, 3)) == List(0, 1, 2, 3))
        assert(Lists.compress[Int](List(0)) == List(0))
        assert(Lists.compress[List[Int]](List(List(1,2,3), List(1,2,3), List(4,5,6))) == List(List(1, 2, 3), List(4,5,6)))
      }
    }
    describe("the solution to compress") {
      it("should return pack List") {
        assert(Lists.pack(List()) == Nil)
        assert(Lists.pack(List(1)) == List(List(1)))
        assert(Lists.pack(List(1, 2)) == List(List(1), List(2)))
        assert(Lists.pack(List("a", "a", "a", "a", "b", "c", "c", "a", "a", "d", "e", "e", "e", "e")) == List(List("a", "a", "a", "a"), List("b"), List("c", "c"), List("a", "a"), List("d"), List("e", "e", "e", "e")))
      }
    }

    describe("the solution to encode") {
      it("should return encode List") {
        assert(Lists.encode(List()) == Nil)
        assert(Lists.encode(List(1)) == List((1, 1)))
        assert(Lists.encode(List(1, 2)) == List((1 ,1), (1, 2)))
        assert(Lists.encode(List("a", "a", "a", "a", "b", "c", "c", "a", "a", "d", "e", "e", "e", "e")) == List((4,"a"), (1,"b"), (2,"c"), (2,"a"), (1, "d"), (4,"e")))
      }
    }

    describe("the solution to encodeModified") {
      it("should return encodeModified List") {
        assert(Lists.encodeModified(List()) == Nil)
        assert( Lists.encodeModified(List(1)) == List(Left(1)))
        assert(Lists.encodeModified(List(1, 2)) == List(Left(1), Left(2)))
        assert(
          Lists.encodeModified(List("a", "a", "a", "a", "b", "c", "c", "a", "a", "d", "e", "e", "e", "e")) 
          == List(Right((4,"a")), Left("b"), Right((2,"c")), Right((2,"a")), Left("d"), Right((4,"e")))
          )
      }
    }

    describe("the solution to decode") {
      it("should return decoded List") {
        assert(
          Lists.decode(List((4, "a"), (1, "b"), (2, "c"), (2, "a"), (1, "d"), (4, "e")))
          == List("a", "a", "a", "a", "b", "c", "c", "a", "a", "d", "e", "e", "e", "e")
        )
        assert(Lists.decode(List()) == Nil)
        assert(Lists.decode(List((1, "b"))) == List("b"))
        assert(Lists.decode(List((1, 2), (2, 3))) == List(2, 3, 3))
        assert(
          Lists.decode(
            List(
              (2, List("a", "a", "a")), (3, "BC"), (1, List("a", 3))
            )
          ) == List(List("a", "a", "a"), List("a", "a", "a"), "BC", "BC", "BC", List("a", 3))
        )
      }
    }


    describe("the solution to encodeDirectEncode") {

      it("should return decoded List") {
        assert(Lists.encodeDirect(List()) == Nil)
        assert(Lists.encodeDirect(List(1)) == List((1, 1)))
        assert(Lists.encodeDirect(List(1, 2)) == List((1 ,1), (1, 2)))
        assert(Lists.encodeDirect(
          List("a", "a", "a", "a", "b", "c", "c", "a", "a", "d", "e", "e", "e", "e")
          ) == List((4,"a"), (1,"b"), (2,"c"), (2,"a"), (1, "d"), (4,"e")))
      }
    }

    describe("the solution to duplicate") {
      it("should return duplicated list") {
        assert(
          Lists.duplicate(List("a", "b", "c", "c", "d")) == List("a", "a", "b", "b", "c", "c", "c", "c", "d", "d")
        )
      }
    }

    describe("the solution to duplicateNHelper") {
      it("should return duplicated list") {

        assert(
          Lists.duplicateN(3, List("a", "b", "c", "c", "d"))
           == List("a", "a","a", "b", "b", "b", "c", "c","c", "c", "c", "c", "d", "d","d")
        )
      }
    }

    describe("the solution to drop") {
      it("should return duplicated list") {

        assert(
          Lists.drop(3, List("a", "b", "c", "d", "e", "f", "g", "h", "i", "j", "k"))
          == List("a", "b", "d", "e", "g", "h", "j", "k")
        )
      }
    }


    describe("the solution to drop") {
      it("should return duplicated list") {
        assert(
          Lists.split(3, List("a", "b", "c", "d", "e", "f", "g", "h", "i", "j", "k"))
          == (List("a", "b", "c"),List("d", "e", "f", "g", "h", "i", "j", "k"))
        )
      }
    }
  }
}
