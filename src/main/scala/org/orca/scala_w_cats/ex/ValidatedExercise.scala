package org.orca.scala_w_cats.ex

import cats.Semigroupal
import cats.data.Validated
import cats.data.Validated.{Invalid, Valid}
import cats.syntax.applicative._
import cats.instances.list._
import cats.syntax.either._

/**
  * exercise 6.4.4 from Scala With Cats
  */
object ValidatedExercise {

  case class User(name: String, age: Int)

  type FormData = Map[String, String]
  type ShortCircuitFail[A] = Either[List[String], A]
  type AggErrFail[A] = Validated[List[String], A]

  def getValue(fieldName: String)(inputMap: FormData): ShortCircuitFail[String] = {

    Either.fromOption(
      inputMap.get(fieldName),
      s"missing $fieldName from form data".pure[List]
    )

  }

  val getNameStr: FormData => ShortCircuitFail[String] = getValue("name")

  val getAgeStr: FormData => ShortCircuitFail[String] = getValue("age")

  def parseInt(name: String)(numStr: String): ShortCircuitFail[Int] = {
    Either.catchOnly[java.lang.NumberFormatException](numStr.toInt).leftMap {
      e => s"$name must be an integer ${e.getMessage}".pure[List]
    }
  }



  def nonBlank(name: String)(value: String): ShortCircuitFail[String] = {
    value.asRight[List[String]].ensure(List(s"$name should be non empty"))(_.nonEmpty)
  }

  def nonNegative(name: String)(value: Int): ShortCircuitFail[Int] = {
    if (value >= 0) Right(value) else "should be non-negative".pure[List].asLeft[Int]
  }

  def readName(formData: FormData): ShortCircuitFail[String] = {
    for {
      rawName <- getValue("name")(formData)
      validatedName <- nonBlank("name")(rawName)
    } yield validatedName
  }

  def readAge(formData: FormData): ShortCircuitFail[Int] = {
    for {
      rawAge <- getValue("age")(formData)
      age <- parseInt("age")(rawAge)
      validatedAge <- nonNegative("age")(age)
    } yield validatedAge
  }

  def validateUser(formData: FormData): AggErrFail[User] = {
    Semigroupal.map2(readName(formData).toValidated, readAge(formData).toValidated)(User.apply)
  }

  def main(args: Array[String]): Unit = {

    validateUser(Map("name" -> "4", "age" -> "1")) match {
      case Invalid(e) => println(e)
      case Valid(a) => println(s"the user is $a")
    }



  }


}
