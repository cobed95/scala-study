package org.laplacetec.study
package problems99

import org.scalatest.funspec.AnyFunSpec

class ListsSpec extends AnyFunSpec {
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

    describe("the solution to P04") {
      it("should return the length") {
        assert(Lists.length(List(0, 1, 2)) == 3)
      }
    }

    describe("the solution to P05") {
      it("should return reversed list") {
        assert(List(0, 1, 2) == Lists.reverse(List(2, 1, 0)))
      }
    }

    describe("the solution to P06") {
      it("should check for palindrome") {
        assert(Lists.isPalindrome(List(0, 1, 0)))
      }

      it("should check not for palindrome") {
        assert(!Lists.isPalindrome(List(0, 1, 2)))
      }
    }

    describe("the solution to flatMap") {
      it("should return flattened list") {
        assert(
          Lists.flatMap[Any, Any](List(List(0), List(1), 2, List(List(3))), {
            case x: List[_] => x
            case x          => List(x)
          }) == List(0, 1, 2, List(3))
        )
      }
    }

    describe("the solution to P07") {
      it("should return flattened list 1") {
        assert(Lists.flatten(List(0)) == List(0))
      }

      it("should return flattened list 2") {
        assert(Lists.flatten(List(0, List(1))) == List(0, 1))
      }

      it("should return flattened list 3") {
        assert(
          Lists.flatten(List(0, List(1), 2, List(3, List(4)))) ==
            List(0, 1, 2, 3, 4)
        )
      }
    }

    describe("the solution to P08") {
      it("should return compressed list") {
        assert(Lists.compress(List(1, 1, 2, 2, 1)) == List(1, 2, 1))
      }
    }

    describe("the solution to P09") {
      it("should return packed list") {
        assert(
          Lists
            .pack(List(1, 1, 2, 2, 1)) == List(List(1, 1), List(2, 2), List(1))
        )
      }
    }

    describe("the solution to P10") {
      it("should return encoded list") {
        assert(
          Lists.encode(List(1, 1, 2, 2, 1)) == List((2, 1), (2, 2), (1, 1))
        )
      }
    }

    describe("the solution to P11") {
      it("should return modified encoded list") {
        assert(
          Lists.encodeModified(List(1, 1, 2, 2, 1)) == List(
            Right((2, 1)),
            Right((2, 2)),
            Left(1)
          )
        )
      }
    }

    describe("the solution to P12") {
      it("should return decoded list") {
        assert(
          Lists.decode(List((2, 1), (2, 2), (1, 1))) == List(1, 1, 2, 2, 1)
        )
      }
    }

    describe("the solution to P13") {
      it("should return directly encoded list") {
        assert(
          Lists
            .encodeDirect(List(1, 1, 2, 2, 1)) == List((2, 1), (2, 2), (1, 1))
        )
      }
    }

    describe("the solution to P14") {
      it("should return duplicated list") {
        assert(Lists.duplicate(List(1, 2, 1)) == List(1, 1, 2, 2, 1, 1))
      }
    }
  }
}
