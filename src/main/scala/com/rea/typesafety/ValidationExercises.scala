package com.rea.typesafety

import scalaz._, Scalaz._

object ValidationExercises {

  def validateKey(key: String, input: Map[String, String]): ValidationNel[ErrorCode, String] = {
    input.get(key) match {
      case Some(v) => Success(v)
      case None => Failure(NonEmptyList(keyNotFound(key)))
    }
  }

  def nameValidation(name: String): ValidationNel[ErrorCode, String] = {
    if(name.isEmpty())
      Failure(NonEmptyList(nameIsEmpty(name)))
    else
      Success(name)
  }

  def passwordStrengthValidation(password: String): ValidationNel[ErrorCode, String] = {
    if("""(\d+)""".r.findAllIn(password).isEmpty)
      Failure(NonEmptyList(passwordTooWeak))
    else
      Success(password)
  }

  def passwordLengthValidation(password: String): ValidationNel[ErrorCode, String] = {
    if(password.length() > 0)
      Success(password)
    else
      Failure(NonEmptyList(passwordTooShort))
  }

  def validateInput(input: Map[String, String]): ValidationNel[ErrorCode, Person] = {
    val firstNameVal = validateKey("firstName", input)
    val firstName = firstNameVal.fold(e => firstNameVal, success => nameValidation(success))
    val lastNameVal = validateKey("lastName", input)
    val lastName = lastNameVal.fold(e => lastNameVal, success => nameValidation(success))
    val passwordVal = validateKey("password", input)
    val password = passwordVal.fold(e => passwordVal, success => success.successNel)
    (firstName.ap(lastName).ap(password)) { Person(firstName, lastName, password) }

    // for {
    //   firstName <- validateKey("firstName", input).flatMap(nameValidation)
    //   lastName <- validateKey("lastName", input).flatMap(nameValidation)
    //   password <- validateKey("password", input).flatMap(p => passwordLengthValidation(p) <* passwordStrengthValidation(p))
    // } yield Person(firstName, lastName, password)
  }
}

case class Person(firstName: String, lastName: String, password: String)

sealed trait ErrorCode

case object passwordTooShort extends ErrorCode

case object passwordTooWeak extends ErrorCode

case class keyNotFound(key: String) extends ErrorCode

case class nameIsEmpty(key: String) extends ErrorCode


/*

Interesting Validator combinators

scala> val a:ValidationNel[String,String]  = "hi".successNel
a: scalaz.ValidationNel[String,String] = Success(hi)

scala> val b:ValidationNel[String,String]  = "world".successNel
b: scalaz.ValidationNel[String,String] = Success(world)

scala> val c:ValidationNel[String,String]  = "error1".failNel
c: scalaz.ValidationNel[String,String] = Failure(NonEmptyList(error1))

scala> val d:ValidationNel[String,String]  = "error2".failNel
d: scalaz.ValidationNel[String,String] = Failure(NonEmptyList(error2))

scala> a <* b
res0: scalaz.Validation[scalaz.NonEmptyList[String],String] = Success(hi)

scala> a *> b
res1: scalaz.Validation[scalaz.NonEmptyList[String],String] = Success(world)

scala> c <* d
res2: scalaz.Validation[scalaz.NonEmptyList[String],String] = Failure(NonEmptyList(error1, error2))

scala> a <* d
res3: scalaz.Validation[scalaz.NonEmptyList[String],String] = Failure(NonEmptyList(error2))

scala> a flatMap (hi => b)
res4: scalaz.Validation[scalaz.NonEmptyList[String],String] = Success(world)

scala> a flatMap (s => if (s == "hi") "hey back".successNel else "fine, be that way!".failNel)
res6: scalaz.Validation[scalaz.NonEmptyList[String],String] = Success(hey back)

scala> d flatMap (s => if (s == "hi") "hey back".successNel else "fine, be that way!".failNel)
res7: scalaz.Validation[scalaz.NonEmptyList[String],String] = Failure(NonEmptyList(error2))

scala> b flatMap (s => if (s == "hi") "hey back".successNel else "fine, be that way!".failNel)
res8: scalaz.Validation[scalaz.NonEmptyList[String],String] = Failure(NonEmptyList(fine, be that way!))

scala> a map (hi => hi + " worldz")
res5: scalaz.Validation[scalaz.NonEmptyList[String],String] = Success(hi worldz)


 */
