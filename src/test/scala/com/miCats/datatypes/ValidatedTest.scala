package com.miCats.datatypes

import org.scalatest.FunSuite

class ValidatedTest extends FunSuite {

  test("I can transform from EitherT to ValidatedNel") {
    import cats.data.Validated.Valid
    import cats.data.{EitherT, ValidatedNel}
    import cats.implicits._

    val eitherT: EitherT[Option, String, Int] = EitherT.right(Some(2))

    val validateNel: Option[ValidatedNel[String, Int]] = eitherT.toValidatedNel

    assertResult(Some(Valid(2))) {
      validateNel
    }
  }

  test("I can transform from Validated Nel's Valid to EitherT") {
    import cats.data.{EitherT, ValidatedNel, _}
    import cats.implicits._

    val validateNel: Option[ValidatedNel[String, Int]] = Some(2.validNel)

    val eitherT: EitherT[Option, NonEmptyList[String], Int] = EitherT(validateNel.map(_.toEither))

    assertResult(EitherT.right(Option(2))) {
      eitherT
    }
  }

  test("I can transform from ValidatedNel's Invalid to EitherT") {
    import cats.data.{EitherT, ValidatedNel, _}
    import cats.implicits._

    val validateNel: Option[ValidatedNel[String, Int]] = Some("fail".invalidNel)

    val eitherT: EitherT[Option, NonEmptyList[String], Int] = EitherT(validateNel.map(_.toEither))

    assertResult(EitherT.left(Option(NonEmptyList("fail", Nil)))) {
      eitherT
    }
  }

}
