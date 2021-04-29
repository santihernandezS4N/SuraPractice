package com.miCats.datatypes

import org.scalatest.AsyncFunSuite

class NestedTest extends AsyncFunSuite {

  test("Nested allows us to compose two map operations into one. " +
    "List and Option") {
    import cats.data._
    import cats.implicits._

    val listOptional: List[Option[Int]] = List(Some(1), Some(2), None, Some(4))

    val nested: Nested[List, Option, Int] = Nested(listOptional)

    assert(nested.map(_ * 2).contains_(8))
  }

  test("Nested allows us to compose two map operations into one. " +
    "Future and Either's Right") {
    import cats.data.Nested
    import cats.implicits._
    import scala.concurrent.Future

    val success: Future[Either[String, Int]] = Future.successful(Right(2))
    val nestedSuccess = Nested(success)
    val operationOverNested = nestedSuccess.map(_ * 3)

    operationOverNested.value.map(_ match {
      case Right(6) => assert(true)
      case Left(_) => assert(false)
    })
  }

  test("Nested allows us to compose two map operations into one. " +
    "Future and Either's Left") {
    import cats.data.Nested
    import cats.implicits._
    import scala.concurrent.Future

    val fail: Future[Either[String, Int]] = Future.successful(Left("fail"))
    val nestedFail = Nested(fail)

    val operationOverNested = nestedFail.map(_ * 3)

    operationOverNested.value.map(either => {
      either match {
        case Right(_) => assert(false)
        case Left(msj) => assert(msj == "fail")
      }
    })
  }


}
