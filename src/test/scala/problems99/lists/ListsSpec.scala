package org.laplacetec.study
package problems99.lists

import org.scalatest.funspec.AnyFunSpec

class ListsSpec extends AnyFunSpec{
  describe("In the lists section of the 99 Problems in Scala,") {
    describe("the solution to P01") {
      it("should return None when list is empty.") {
        assert(P01.solution(List()).isEmpty)
      }

      it("should return the last element.") {
        val toTest = List(0, 1, 2, 3)
        assert(P01.solution(toTest).get == 3)
      }

      it("should work for boolean type too.") {
        val toTest = List(false, false, true)
        assert(P01.solution(toTest).get)
      }
    }

    describe("the solution to P02") {
      it("should return None when list is empty.") {
        assert(P02.solution(List()).isEmpty)
      }

      it("should return None when list has only one element.") {
        assert(P02.solution(List(0)).isEmpty)
      }

      it("should return the first element when the list has two elements.") {
        assert(P02.solution(List(0, 1)).get == 0)
      }

      it("should return the truly penultimate element.") {
        assert(P02.solution(List(0, 1, 2, 3)).get == 2)
      }
    }

    describe("the solution to P03") {
      it("should return None if index >= list len") {
        assert(P03.solution(4, List(0, 1, 2, 3)).isEmpty)
      }

      it("should return None if index < 0") {
        assert(P03.solution(-1, List(0, 1, 2, 3)).isEmpty)
      }

      it("should return nth element properly") {
        List(0, 1, 2, 3) foreach (index => {
          assert(P03.solution(index, List(0, 1, 2, 3)).get == index)
        })
      }
    }
  }
}
