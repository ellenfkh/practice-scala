package org.scalalabs.basic.lab02

import scala.collection.mutable.ListBuffer
 import sys._


object ListManipulationExercise02 {

  /**
   * Find the maximum element in a list, e.g. maxElementInList(List(1,9,3,5)) == 9
   * As usual, various ways exist: pattern matching, folding, ...
   */
  def maxElementInList(l: List[Int]): Int = {
    l.foldLeft(0) { (currMax, i) =>
      if (currMax > i) {
        currMax
      } else {
        i
      }
    }
  }

  /**
   * Calculate the sum of the equally position elements
   * of the two list
   */
  def sumOfTwo(l1: List[Int], l2: List[Int]): List[Int] = {
    if (l1.isEmpty) {
      l2
    } else if (l2.isEmpty) {
      l1
    } else {
      List(l1.head + l2.head) ::: sumOfTwo(l1.tail, l2.tail)
    }
  }

  /**
   *  For this exercise preferably make use of the sumOfTwo
   * method above
   */
  def sumOfMany(l: List[Int]*): List[Int] = {
    l.foldLeft(List.empty[Int]) { (list, next) =>
      sumOfTwo(list, next)
    }
  }

  case class Person(age: Int, firstName: String, lastName: String)

  /**
   * The following method is implemented in the most in-elegant way we could think of.
   * The idea is to re-write the method into more functional style. In the end, you
   * may be able to achieve the same functionality as implemented below
   * in a one-liner.
   */
  def separateTheYoungFromTheOld(persons: List[Person]): List[List[String]] = {
    // fancy but disgusting.
    List(persons.filter(p => p.age < 18), persons.filter(p => p.age >= 18)).map(l => l.map(p => p.firstName).sorted)
  }

}
