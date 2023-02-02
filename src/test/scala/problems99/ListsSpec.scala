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
  }
}
