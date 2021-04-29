package com.miCats.datatypes

import org.scalatest.AsyncFunSuite

import scala.concurrent.Future

class EitherTest extends AsyncFunSuite {


  test("We can combine two Future[Either]") {
    val subtrahend: Future[Either[String, Int]] = Future.successful(Right(18))
    val minuend: Future[Either[String, Int]] = Future.successful(Right(7))

    val result: Future[Either[String, Int]] = combine(subtrahend, minuend)((v1: Int, v2: Int) => v1 - v2)((f1: String, f2: String) => f1 + " " + f2)

    result.map(_ match {
      case Right(11) => assert(true)
      case _ => assert(false)
    })
  }

  test("We can combine two EitherT") {
    import cats.data.EitherT
    import cats.implicits._
    val subtrahend: EitherT[Future, String, Int] = EitherT.right(Future.successful(18))
    val minuend: EitherT[Future, String, Int] = EitherT.right(Future.successful(7))

    val result: Future[Either[String, Int]] = combine(subtrahend.value, minuend.value)((v1: Int, v2: Int) => v1 - v2)((f1: String, f2: String) => f1 + " " + f2)

    result.map(_ match {
      case Right(11) => assert(true)
      case _ => assert(false)
    })
  }

  test("We can combine two ValidatedNel like EitherT") {
    import cats.data.{ValidatedNel, _}
    import cats.implicits._

    val subtrahend: Future[ValidatedNel[String, Int]] = Future.successful(18.validNel)
    val minuend: Future[ValidatedNel[String, Int]] = Future.successful(7.validNel)

    val fRight = (v1: Int, v2: Int) => v1 - v2
    val fLeft = (f1: NonEmptyList[String], f2: NonEmptyList[String]) => f1 ++ f2.toList

    val result: Future[Either[NonEmptyList[String], Int]] = combine(subtrahend.map(_.toEither), minuend.map(_.toEither))(fRight)(fLeft)

    result.map(_ match {
      case Right(11) => assert(true)
      case _ => assert(false)
    })
  }


  def combine[L, R, B](v1: Future[Either[L, R]], v2: Future[Either[L, R]])(fr: (R, R) => B)(fl: (L, L) => L): Future[Either[L, B]] = {
    for {
      one <- v1
      two <- v2
    } yield {
      (one, two) match {
        case (Right(value1), Right(value2)) => Right(fr(value1, value2))
        case (Right(_), Left(fail2)) => Left(fail2)
        case (Left(fail1), Right(_)) => Left(fail1)
        case (Left(fail1), Left(fail2)) => Left(fl(fail1, fail2))
      }
    }
  }

  def combine2[L, R, B](v1: Future[Either[L, R]], v2: Future[Either[L, R]])(fr: (R, R) => B): Future[Either[L, B]] = {
    for {
      one <- v1
      two <- v2
    } yield {
      for {
        o <- one
        t <- two
      } yield {
        fr(o, t)
      }
    }
  }

}
