package org.orca.scala_w_cats.ex

import cats.Monoid

/**
  * exercise 7.1.3 from Scala With Cats
  */
object FunctionsInTermOfFoldRight {

  def map[A, B](la: List[A])(fab: A => B): List[B] = {
    la.foldRight(List.empty[B])((a, listB) => fab(a) :: listB)
  }

  def flatMap[A,B](la: List[A])(fab: A => List[B]): List[B] = {
    la.foldRight(List.empty[B])((a, listB) => fab(a) ++ listB)
  }

  def filter[A](la: List[A])(p: A => Boolean): List[A] = {
    la.foldRight(List.empty[A]){(a, listA) =>
      if (p(a)) a :: listA else listA
    }

  }


  def sum[A](la: List[A])(implicit num: Numeric[A]): A = {
    la.foldRight(num.zero)((a, sumA) => num.plus(a, sumA))
  }

  def monoidSum[A](la: List[A])(implicit monoid: Monoid[A]): A = {
    la.foldRight(monoid.empty)(monoid.combine)
  }

  def main(args: Array[String]): Unit = {
    println(flatMap(List(1, 2, 3))(a => List(a, a * 10, a * 100)))
  }


}
