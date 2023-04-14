package org.laplacetec.study
package problems99

import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers

class ListsSpec extends AnyFunSpec with Matchers {
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

    describe("slice") {
      it("should return an empty list if the input list is empty") {
        val actual = Lists.slice(0, 1, Nil)
        val expected = Nil
        assert(actual == expected)
      }

      it("should return an empty list if the start index is greater than or equal to the end index") {
        val actual1 = Lists.slice(1, 1, List(1, 2, 3))
        val expected1 = Nil
        assert(actual1 == expected1)

        val actual2 = Lists.slice(3, 1, List(1, 2, 3))
        val expected2 = Nil
        assert(actual2 == expected2)
      }

      it("should return a slice of the list starting from the start index up to but not including the end index") {
        val actual = Lists.slice(2, 5, List(1, 2, 3, 4, 5, 6, 7))
        val expected = List(3, 4, 5)
        assert(actual == expected)
      }

      it("should return the entire list if the start index is 0 and the end index is the length of the list") {
        val actual = Lists.slice(0, 4, List(1, 2, 3, 4))
        val expected = List(1, 2, 3, 4)
        assert(actual == expected)
      }

      it("should return a slice starting from the start index if the end index is greater than the length of the list") {
        val actual = Lists.slice(2, 10, List(1, 2, 3, 4))
        val expected = List(3, 4)
        assert(actual == expected)
      }

      it("should return a slice up to but not including the end index if the end index is equal to the length of the list") {
        val actual = Lists.slice(0, 3, List(1, 2, 3, 4))
        val expected = List(1, 2, 3)
        assert(actual == expected)
      }
    }

    describe("switchConcat") {

      it("should concatenate two lists in reverse order") {
        assert(Lists.switchConcat(List(1, 2, 3), List(4, 5, 6)) == List(4, 5, 6, 1, 2, 3))
      }

      it("should return an empty list when both lists are empty") {
        assert(Lists.switchConcat(List.empty[Int], List.empty[Int]) == List.empty[Int])
      }

      it("should return the second list when the first list is empty") {
        assert(Lists.switchConcat(List.empty[Int], List(1, 2, 3)) == List(1, 2, 3))
      }

      it("should return the first list when the second list is empty") {
        assert(Lists.switchConcat(List(1, 2, 3), List.empty[Int]) == List(1, 2, 3))
      }
    }

    describe("untuple") {

      it("should return a function that takes a tuple as input") {
        val f = Lists.untuple((t: (Int, String)) => t._2)
        assert(f(1, "hello") == "hello")
      }

      it("should return the result of applying the input function to the tuple") {
        val f = Lists.untuple((t: (Int, String)) => t._2.toUpperCase)
        assert(f(1, "hello") == "HELLO")
      }
    }

    describe("rotate") {

      it("should rotate a list N places to the left") {
        assert(Lists.rotate(3, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k)) ==
          List('d, 'e, 'f, 'g, 'h, 'i, 'j, 'k, 'a, 'b, 'c))
        assert(Lists.rotate(-2, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k)) ==
          List('j, 'k, 'a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i))
      }

      it("should return an empty list when the input list is empty") {
        assert(Lists.rotate(3, Nil) == Nil)
      }

      it("should return the input list when N is zero") {
        assert(Lists.rotate(0, List(1, 2, 3)) == List(1, 2, 3))
      }
    }

    describe("removeAt") {

      it("should remove the Kth element from a list") {
        assert(Lists.removeAt(1, List('a, 'b, 'c, 'd)) == (List('a, 'c, 'd), 'b))
      }

      it("should throw an exception when the list is empty") {
        assertThrows[ArrayIndexOutOfBoundsException] {
          Lists.removeAt(0, Nil)
        }
      }

      it("should throw an exception when K is out of bounds") {
        assertThrows[ArrayIndexOutOfBoundsException] {
          Lists.removeAt(4, List('a, 'b, 'c, 'd)) == (List('a, 'b, 'c, 'd), null)
        }
      }
    }

    describe("insertAt") {
      it("should insert an element at the beginning of the list") {
        assert(Lists.insertAt('new, 0, List('a, 'b, 'c)) == List('new, 'a, 'b, 'c))
      }
      it("should insert an element at the end of the list") {
        assert(Lists.insertAt('new, 3, List('a, 'b, 'c)) == List('a, 'b, 'c, 'new))
      }
      it("should insert an element in the middle of the list") {
        assert(Lists.insertAt('new, 2, List('a, 'b, 'c)) == List('a, 'b, 'new, 'c))
      }
      it("should return the original list if the index is greater than the list length") {
        assert(Lists.insertAt('new, 5, List('a, 'b, 'c)) == List('a, 'b, 'c, 'new))
      }
      it("should return a list with the new element if the index is negative") {
        assert(Lists.insertAt('new, -1, List('a, 'b, 'c)) == List('new, 'a, 'b, 'c))
      }
    }

    describe("range") {
      it("should return an empty list if the start is greater than the end") {
        assert(Lists.range(5, 2) == Nil)
      }
      it("should return a list with all integers within a given range") {
        assert(Lists.range(1, 5) == List(1, 2, 3, 4, 5))
      }
    }

    describe("randomSelect") {
      it("should return an empty list if the input list is empty") {
        assert(Lists.randomSelect(3, Nil) == Nil)
      }
      it("should return an empty list if n is less than or equal to 0") {
        assert(Lists.randomSelect(0, List('a, 'b, 'c)) == Nil)
        assert(Lists.randomSelect(-1, List('a, 'b, 'c)) == Nil)
      }
      it("should return n randomly selected elements from the input list") {
        val list = List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j)
        val selected = Lists.randomSelect(5, list)
        assert(selected.length == 5)
        assert(selected.toSet.subsetOf(list.toSet))
      }
    }

    describe("lotto") {
      it("should return an empty list if n is zero") {
        Lists.lotto(0, 10) shouldBe Nil
      }

      it("should return a list of length n") {
        val n = 5
        val result = Lists.lotto(n, 10)
        result.length shouldBe n
      }

      it("should return a list of distinct integers") {
        val result = Lists.lotto(10, 20)
        result.distinct.length shouldBe result.length
      }

      it("should return a list of integers between 1 and m (inclusive)") {
        val m = 10
        val result = Lists.lotto(5, m)
        result.forall(n => n >= 1 && n <= m) shouldBe true
      }
    }

    describe("randomPermute") {
      it("should return an empty list if the input list is empty") {
        val result = Lists.randomPermute(List.empty[Int])
        result shouldBe Nil
      }

      it("should return a list of the same length as the input list") {
        val input = List(1, 2, 3, 4, 5)
        val result = Lists.randomPermute(input)
        result.length shouldBe input.length
      }

      it("should return a list with the same elements as the input list") {
        val input = List(1, 2, 3, 4, 5)
        val result = Lists.randomPermute(input)
        result.toSet shouldBe input.toSet
      }

      it("should return a different permutation each time it is called") {
        val input = List(1, 2, 3, 4, 5)
        val result1 = Lists.randomPermute(input)
        val result2 = Lists.randomPermute(input)
        result1 should not be result2
      }
    }

    describe("combinations") {
      it("should return an empty list if n is zero") {
        Lists.combinations(0, List(1, 2, 3)) shouldBe Nil
      }

      it("should return an empty list if n is greater than the length of the input list") {
        Lists.combinations(4, List(1, 2, 3)) shouldBe Nil
      }

      it("should return a list with one element if n is 1") {
        val input = List(1, 2, 3, 4, 5)
        val result = Lists.combinations(1, input)
        result.length shouldBe input.length
        result.foreach(sublist => sublist.length shouldBe 1)
      }

      it("should return all combinations of k elements from the input list") {
        val input = List(1, 2, 3)
        val result = Lists.combinations(2, input)
        result should contain theSameElementsAs List(List(1, 2), List(1, 3), List(2, 3))
      }

      it("should return all combinations in lexicographic order") {
        val input = List(1, 2, 3)
        val result = Lists.combinations(2, input)
        result shouldBe List(List(1, 2), List(1, 3), List(2, 3))
      }
    }
  }
}
