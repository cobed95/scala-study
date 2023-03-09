package org.laplacetec.study
package problems99

import org.laplacetec.study.problems99.Lists.{compress, decode, duplicate, encodeDirect, encodeModified, insertAt, removeAt, rotate, slice, range}
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

      describe("the solution to P04") {
        it("should return length of list if length > 0") {
          val toTest = List(0, 1, 2, 3)
          assert(Lists.length(toTest) == toTest.length)
        }

        it("should return 0 if list is empty") {
          assert(Lists.length(Nil) == 0)
        }

      }

      describe("the solution to P05") {
        it("should return reverse of list if length > 0") {
          val toTest = List(0, 1, 2, 3)
          assert(Lists.reverse(toTest) == toTest.reverse)
        }

        it("should return Nil if list is empty") {
          assert(Lists.reverse(Nil) == Nil)
        }

      }

      describe("the solution to P06") {
        it("should return true if list is palindrome") {
          assert(Lists.isPalindrome(List(1, 2, 3, 2, 1)))
        }

        it("should return true if list isn't palindrome") {
          assert(!Lists.isPalindrome(List(1, 2, 3, 4, 5)))
        }
      }

      describe("the solution to P07") {
        it("should return an empty list for an empty list") {
          assert(Lists.flatten(List()) == List())
        }

        it("should return a flat list for a list with one element") {
          assert(Lists.flatten(List(1)) == List(1))
        }

        it("should return a flat list for a list with multiple elements") {
          assert(Lists.flatten(List(1, 2, List(3, 4))) == List(1, 2, 3, 4))
        }

        it("should return a flat list for a list with nested lists") {
          assert(Lists.flatten(List(List(1, 2), List(3, List(4, 5)))) == List(1, 2, 3, 4, 5))
        }

        it("should return a flat list for a list with multiple levels of nesting") {
          assert(Lists.flatten(List(List(List(1), 2), List(3, List(4, 5)), 6, List(List(7, 8), 9))) == List(1, 2, 3, 4, 5, 6, 7, 8, 9))
        }
      }

      describe("the solution to P08") {
        it("should return an empty list when given an empty list") {
          assert(Lists.compress(Nil) == Nil)
        }

        it("should return a list with a single element when given a list with a single element") {
          assert(Lists.compress(List(1)) == List(1))
        }

        it("should remove consecutive duplicates") {
          assert(Lists.compress(List(1, 1, 1, 2, 3, 3)) == List(1, 2, 3))
        }

        it("should work for symbols as well as numbers") {
          assert(Lists.compress(List(Symbol("a"), Symbol("a"), Symbol("a"), Symbol("b"), Symbol("c"), Symbol("c"), Symbol("a"), Symbol("a"), Symbol("d"), Symbol("e"), Symbol("e"), Symbol("e"), Symbol("e"))) == List(Symbol("a"), Symbol("b"), Symbol("c"), Symbol("a"), Symbol("d"), Symbol("e")))
        }
      }

      describe("the solution to P09") {
        it("should return an empty list for an empty list") {
          assert(Lists.pack(List()) == List())
        }

        it("should return a list of single-element sublists for a list with unique elements") {
          assert(Lists.pack(List(Symbol("a"), Symbol("b"), Symbol("c"))) == List(List(Symbol("a")), List(Symbol("b")), List(Symbol("c"))))
        }

        it("should return a list of multiple-element sublists for a list with consecutive duplicates") {
          assert(Lists.pack(List(Symbol("a"), Symbol("a"), Symbol("b"), Symbol("b"), Symbol("b"), Symbol("c"), Symbol("c"))) == List(List(Symbol("a"), Symbol("a")), List(Symbol("b"), Symbol("b"), Symbol("b")), List(Symbol("c"), Symbol("c"))))
        }

        it("should return a list of sublists of correct lengths for a list with a mix of consecutive duplicates and unique elements") {
          assert(Lists.pack(List(Symbol("a"), Symbol("a"), Symbol("b"), Symbol("c"), Symbol("c"), Symbol("d"), Symbol("d"), Symbol("d"), Symbol("d"))) == List(List(Symbol("a"), Symbol("a")), List(Symbol("b")), List(Symbol("c"), Symbol("c")), List(Symbol("d"), Symbol("d"), Symbol("d"), Symbol("d"))))
        }
      }

      describe("the solution to P11") {
        it("should return an empty list for an empty list") {
          assert(encodeModified(List()) == List())
        }

        it("should return a list of single-element sublists for a list with unique elements") {
          assert(encodeModified(List('a', 'b', 'c')) == List(Left('a'), Left('b'), Left('c')))
        }

        it("should return a list of (count, element) tuples for a list with consecutive duplicates") {
          assert(encodeModified(List('a', 'a', 'b', 'b', 'b', 'c', 'c')) == List(Right((2,'a')), Right((3,'b')), Right((2,'c'))))
        }

        it("should return a list of (count, element) tuples and single-element sublists for a list with a mix of consecutive duplicates and unique elements") {
          assert(encodeModified(List('a', 'a', 'b', 'c', 'c', 'd', 'd', 'd', 'd')) == List(Right((2,'a')), Left('b'), Right((2,'c')), Right((4,'d'))))
        }
      }
    }

    describe("the solution to P12") {
      it("should return the original list for a list with no consecutive duplicates") {
        assert(decode(List((1,'a'), (1,'b'), (1,'c'))) == List('a', 'b', 'c'))
      }

      it("should return a list with consecutive duplicates for a list with (count, element) tuples") {
        assert(decode(List((2,'a'), (3,'b'), (2,'c'))) == List('a', 'a', 'b', 'b', 'b', 'c', 'c'))
      }

      it("should return a list with consecutive duplicates and unique elements for a list with mixed (count, element) tuples and single-element lists") {
        assert(decode(List((2,'a'), (1,'b'), (2,'c'), (4,'d'), (1,'e'))) == List('a', 'a', 'b', 'c', 'c', 'd', 'd', 'd', 'd', 'e'))
      }
    }
  }

  describe("the solution to P13") {
    it("should return a list of tuples with a count and element for a list with unique elements") {
      assert(encodeDirect(List('a', 'b', 'c')) == List((1, 'a'), (1, 'b'), (1, 'c')))
    }

    it("should return a list of tuples with counts and elements for a list with consecutive duplicates") {
      assert(encodeDirect(List('a', 'a', 'b', 'b', 'b', 'c', 'c')) == List((2, 'a'), (3, 'b'), (2, 'c')))
    }

    it("should return a list of tuples with counts and elements for a list with non-consecutive duplicates") {
      assert(encodeDirect(List('a', 'b', 'a', 'c', 'c', 'a', 'a')) == List((1, 'a'), (1, 'b'), (1, 'a'), (2, 'c'), (2, 'a')))
    }
  }

  describe("the solution to P14") {
    it("should return a list with each element duplicated once for a list with unique elements") {
      assert(duplicate(List('a', 'b', 'c')) == List('a', 'a', 'b', 'b', 'c', 'c'))
    }
  }

  describe("slice function") {
    it("should return the correct sublist for a list with valid indices") {
      assert(slice(List(1, 2, 3, 4, 5, 6, 7, 8, 9), 2, 5) == List(3, 4, 5))
    }

    it("should return the correct sublist for a list with valid indices2") {
      assert(slice(List('a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j'), 3, 7) == List('d', 'e', 'f', 'g'))
    }
  }

  describe("rotate function") {
    it("should correctly rotate a non-empty list by a positive amount") {
      assert(rotate(3, List('a', 'b', 'c', 'd', 'e', 'f', 'g', 'h')) == List('d', 'e', 'f', 'g', 'h', 'a', 'b', 'c'))
    }

    it("should correctly rotate a non-empty list by a negative amount") {
      assert(rotate(-2, List('a', 'b', 'c', 'd', 'e', 'f', 'g', 'h')) == List('g', 'h', 'a', 'b', 'c', 'd', 'e', 'f'))
    }

    it("should return the original list when rotating by zero") {
      assert(rotate(0, List(1, 2, 3)) == List(1, 2, 3))
    }

    it("should return the original list when rotating by a multiple of the list length") {
      assert(rotate(6, List(1, 2, 3, 4, 5)) == List(1, 2, 3, 4, 5))
    }
  }

  describe("removeAt function") {
    it("should remove the element at the given index and return the result as a tuple") {
      assert(removeAt(1, List('a', 'b', 'c', 'd', 'e')) == ('b', List('a', 'c', 'd', 'e')))
      assert(removeAt(3, List(1, 2, 3, 4, 5)) == (4, List(1, 2, 3, 5)))
    }
  }

  describe("insertAt function") {
    it("should insert an element at the given index and return the resulting list") {
      assert(insertAt('X', 1, List('a', 'b', 'c', 'd')) == List('a', 'X', 'b', 'c', 'd'))
      assert(insertAt(10, 0, List(1, 2, 3, 4)) == List(10, 1, 2, 3, 4))
      assert(insertAt("foo", 2, List("bar", "baz", "qux")) == List("bar", "baz", "foo", "qux"))
    }
  }

  describe("range function") {
    it("should generate a list of integers from a starting value to an ending value") {
      assert(range(1, 5) == List(1, 2, 3, 4, 5))
      assert(range(-3, 3) == List(-3, -2, -1, 0, 1, 2, 3))
      assert(range(0, 0) == List(0))
    }
  }
}
