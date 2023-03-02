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

    describe("the solution to P04") {
      it("should return 0 when the list is empty.") {
        assert(Lists.length(List()) == 0)
      }

      it("should return the length properly.") {
        assert(Lists.length(List(0, 1, 2, 3)) == 4)
      }
    }

    describe("the solution to P05") {
      it("should reverse empty lists properly.") {
        assert(Lists.reverse(List()).isEmpty)
      }

      it("should reverse lists properly.") {
        assert(Lists.reverse(List(0, 1, 2, 3)) == List(3, 2, 1, 0))
      }
    }

//    describe("the solution to P06") {
//      it("should return true when the list is empty.") {
//        assert(Lists.isPalindrome(List()))
//      }
//
//      it("should return true when the list has a single element") {
//        assert(Lists.isPalindrome(List(0)))
//      }
//
//      it("should return true when the list is a palindrome with odd number of elements") {
//        assert(Lists.isPalindrome(List(1, 2, 3, 2, 1)))
//      }
//
//      it("should return true when the list is a palindrome with even number of elements") {
//        assert(Lists.isPalindrome(List(1, 2, 3, 3, 2, 1)))
//      }
//
//      it("should return false when the list is not a palindrome") {
//        assert(!Lists.isPalindrome(List(1, 2, 3, 4)))
//      }
//    }
    describe("Lists.duplicateN") {
      it("should return empty list when input is empty list") {
        assert(Lists.duplicateN(3, List()) === List())
      }

      it("should return the original list when n is 1") {
        assert(Lists.duplicateN(1, List(1, 2, 3)) === List(1, 2, 3))
      }

      it("should duplicate the elements of the list n times") {
        assert(Lists.duplicateN(3, List(1, 2, 3)) === List(1, 1, 1, 2, 2, 2, 3, 3, 3))
      }

      it("should handle duplicate elements in the input list") {
        assert(Lists.duplicateN(2, List('a', 'b', 'b', 'c')) === List('a', 'a', 'b', 'b', 'b', 'b', 'c', 'c'))
      }
    }

    describe("drop") {
      it("should return empty list when input is empty list") {
        assert(Lists.drop(2, List()) === List())
      }

      it("should return an empty list when n is 1") {
        assert(Lists.drop(1, List(1, 2, 3)) === List())
      }

      it("should return the original list when n is less than or equal to 0") {
        assert(Lists.drop(0, List(1, 2, 3)) === List(1, 2, 3))
        assert(Lists.drop(-1, List(1, 2, 3)) === List(1, 2, 3))
      }

      it("should Lists.drop every nth element from the list") {
        assert(Lists.drop(3, List(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)) === List(1, 2, 4, 5, 7, 8, 10))
      }

      it("should return the same list when n is greater than the list size") {
        assert(Lists.drop(10, List(1, 2, 3)) === List(1, 2, 3))
      }
    }

    describe("split") {
      it("should return empty lists when input list is empty") {
        assert(Lists.split(3, List()) === (List(), List()))
      }

      it("should Lists.split the list into two parts with the first part having n elements") {
        assert(Lists.split(3, List(1, 2, 3, 4, 5, 6)) === (List(1, 2, 3), List(4, 5, 6)))
      }

      it("should return the original list as the second part when n is greater than or equal to the length of the list") {
        assert(Lists.split(10, List(1, 2, 3, 4, 5)) === (List(1, 2, 3, 4, 5), List()))
        assert(Lists.split(5, List(1, 2, 3)) === (List(1, 2, 3), List()))
      }

      it("should handle duplicate elements in the input list") {
        assert(Lists.split(2, List('a', 'b', 'b', 'c', 'd')) === (List('a', 'b'), List('b', 'c', 'd')))
      }
    }
  }
}
