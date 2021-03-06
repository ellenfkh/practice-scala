package org.scalalabs.basic.lab03
import sys._
/**
 * This exercise introduces you to pattern matching in combination with recursion.
 *
 * Recursion is a key concept for the functional style programming.
 * In the exercises below you learn how to apply recursion in combination with Scala's pattern matching facilities.
 *
 * For this exercise exclusively use pattern matching constructs in order to make the corresponding unittest work.
 *
 * Reference material to solve these exercises can be found here:
 * Pattern matching in general: http://programming-scala.labs.oreilly.com/ch03.html#PatternMatching
 * Pattern matching and recursion: http://programming-scala.labs.oreilly.com/ch08.html#Recursion
 */

object RecursionPatternMatchingExercise {

  /**
   * ***********************************************************************
   * Recursive algorithms with pattern matching
   * For expected solution see unittest @RecursionPatternMatchingExerciseTest
   * ***********************************************************************
   */
  /**
   * Create a method that checks that each subsequent value is greater than
   * the previous one.
   * E.g.:
   * checkValuesIncrease(Seq(1,2,3)) == true
   * checkValuesIncrease(Seq(1,2,2)) == false
   */
  def checkValuesIncrease(seq: Seq[Int]): Boolean = {
    seq match {
      case s if s.length < 2 => true
      case s if s.head < s.tail.head => checkValuesIncrease(s.tail)
      case _ => false
    }
  }

  /**
   * Group Consecutive values
   * List(1,1,2,3,1,1) -> List(1,1), List(2), List(3), List(1,1)
   */
  def groupConsecutive[T](in: List[T]): List[List[T]] = {
    in match {
      case l if l.length < 2 => List(l)
      case l if l.head == l.tail.head => List(List(l.head) :::
        groupConsecutive(l.tail).head) ::: groupConsecutive(l.tail).tail
      case l => List(List[T](l.head)) ::: groupConsecutive(l.tail)
    }
  }

  /**
   * Group Equal values
   * List(1,1,2,3,1,1) -> List(1,1,1,1), List(2), List(3)
   */
  def groupEquals[T](in: List[T]): List[List[T]] = {
    in match {
      case l if l.isEmpty => List.empty[List[T]]
      case l => l.filter(x => x == l.head) :: groupEquals(l.filter(x => x !=
        l.head))
    }
  }

  /**
   * Compress values
   * List(1,1,2,3,1,1) -> List(1,2,3)
   */
  def compress[T](in: List[T]): List[T] = {
    in match {
      case l if l.isEmpty => List.empty[T]
      case l => l.filter(x => x == l.head).head :: compress(l.filter(x => x !=
        l.head))
    }
  }

  /**
   * Define the amount of all equal members
   * List(1,1,2,3,1,1) -> List((4,1),(1,2),(1,3))
   */
  def amountEqualMembers[T](in: List[T]): List[(Int, T)] = {
    in match {
      case l if l.isEmpty => List.empty[(Int, T)]
      case l => (l.filter(x => x == l.head).length, l.head) :: amountEqualMembers(l.filter(x => x != l.head))
    }
  }

  /**
   * Zip multiple lists
   * List(List(1,2,3), List('A, 'B, 'C), List('a, 'b, 'c)) -> List(List(1, 'A,
     * 'a), List(2, 'B, 'b), List(3, 'C, 'c))
   */
  def zipMultiple(in: List[List[_]]): List[List[_]] = {
    in match {
      case l if l.head.isEmpty => List.empty[List[_]]
      case l => l.map(x => x.head) :: zipMultiple(l.map(x => x.tail))
    }
  }

  /**
   * Zip multiple lists with different sizes
   * List(List(1), List('A, 'B, 'C), List('a, 'b)) -> List(List(1, 'A, 'a))
   */
  def zipMultipleWithDifferentSize(in: List[List[_]]): List[List[_]] = {
    in match {
      case l if (l.map(x => x.isEmpty)).contains(true) => List.empty[List[_]]
      case l => l.map(x => x.head) :: zipMultipleWithDifferentSize(l.map(x => x.tail))
    }
  }
}
