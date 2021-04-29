package com.miCats.typeclasses

import org.scalatest.FunSuite

class TraverseTest extends FunSuite{

  test("Traverse") {

    import cats.Applicative
    import cats.implicits._
    import scala.concurrent.Future
    import scala.concurrent.ExecutionContext.Implicits.global

    case class AclHttp(name: Option[String], age: Option[Int])
    case class Person(name: String, age: Int)

    val request = AclHttp(Some("Peter"), Some(20))

    val x: Option[Person] = Applicative[Option].map2(request.name, request.age)(Person)
    val y = Applicative[Option].map2(request.name, request.age)(Person)
    Future
  }

}
