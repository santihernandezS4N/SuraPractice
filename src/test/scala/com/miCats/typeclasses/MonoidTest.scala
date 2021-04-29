package com.miCats.typeclasses

import org.scalatest.FunSuite

class MonoidTest extends FunSuite {

  /*
  Un Monoid es un type class que tiene:
    * Una operación de combinación (Semigroup)
    * Un valor inicial (identity)

  Un monoid se expresa así:

  trait Semigroup[A] {
    def combine(x: A, y: A): A
  }

  trait Monoid[A] extends Semigroup[A] {
    def empty: A
  }

  Los Monoids obedecen a algunas leyes:
    * Asociativa: El orden de los elementos no altera el resultado de la operación.
        La Suma y la multiplicación obedecen a esta ley. (2*3)*4 = 2*(3*4) => 24
    *


   */

  test("Puedo crear mi propio Monoid de Int, con valor inicial de 0. " +
    "Dado: " +
    " -> Un implicit para sobrescribir el valor inicial de Int. " +
    " -> Una suma de dos números, uno con valor empty y otro con valor de 1. " +
    "Entonces: " +
    " -> El resultado de la suma es 1. ") {

    // Import que permite trabajar con Monoid
    import cats.Monoid

    // Se define el propio Monoid para Int
    implicit val miMonoidParaInt: Monoid[Int] = new Monoid[Int] {
      /*
       El valor inicial o vacio para los Int en este test será
       de 0
      */
      override def empty: Int = 0
      // Función de combinación: Debe cumplir la ley asociativa.
      override def combine(x: Int, y: Int): Int = x + y
    }

    val numero1 = 1
    val numero2: Int = Monoid[Int].empty

    val resultadoSuma = numero1 + numero2
    // 1 + el valor por defecto de Int (en este caso 0) debe ser igual a 1
    assert( resultadoSuma == 1 )
  }

  test("Puedo crear mi propio Monoid de Int para multiplicar un solo elemento. " +
    "Dado: " +
    " -> Un implicit para sobrescribir el valor inicial de Int. " +
    " -> Una multiplicación de una lista con un solo número. " +
    "Entonces: " +
    " -> El resultado de la multiplicación es 2. ") {
    import cats.Monoid
    // se importa para poder trabajar con el |+| como la función de combinación.
    import cats.syntax.semigroup._

    implicit val multiplicacionInt: Monoid[Int] = new Monoid[Int] {
      override def empty: Int = 1
      override def combine(x: Int, y: Int): Int = x * y
    }

    val numerosAMultiplicar: List[Int] = List(2)

    val resultado: Int = numerosAMultiplicar.fold(Monoid[Int].empty)( _ |+| _)

    assert( resultado == 2)

  }

  test("Puedo crear mi propio monoid de Dinero. " +
    "Dado: " +
    " -> Un case clase que representa dinero con un valor interno. " +
    " -> Un implicit de Monoid[Dinero] donde el valor inicial es Dinero con valor 1.0." +
    " -> Una función que suma una lista de Dinero. " +
    "     Con un valor inicial que esta definido por el Monoid[Dinero]. " +
    " -> Dos lista con Dinero. Una vacía y otra con dos instancias del objeto Dinero. " +
    " -> Dos resultados con cada una de las sumas de las dos listas de Dinero. " +
    "Entonces: " +
    " -> La lista con los dos objetos de Dinero da un rasultado con la suma de sus valores internos más " +
    "     el valor inicial establecido (1.0) para el Monoid[Dinero]." +
    " -> La lista sin objetos de Dinero da una sumatoria de sus valores igual al valor inicial " +
    "     establecido por el Monoid[Dinero]. ") {

    // Import que permite trabajar con Monoid
    import cats.Monoid
    // Permite trabajar con la función `combine` pero haciendo referencia a |+|
    import cats.syntax.semigroup._

    // Clase que representa el Dinero con un atributo de tipo Double
    case class Dinero(valor: Double)

    // Se define el propio monoid para Int
    implicit val miMonoidParaDinero: Monoid[Dinero] = new Monoid[Dinero] {
      /*
       El valor inicial o vacio para los Double en este test será
       de 1.0
      */
      override def empty: Dinero = Dinero(1.0)
      // Se implementa la suma de dos Dineros
      override def combine(x: Dinero, y: Dinero): Dinero = Dinero( x.valor + y.valor )
    }

    def acumularMonoides[A: Monoid](elementos: List[A] ): A =
      elementos.foldRight(Monoid[A].empty)((elementoActual, acumulado) =>
        acumulado |+| elementoActual)

    // Lista con varios objetos de Dinero
    val muchoDinero: List[Dinero] = List(Dinero(10.0), Dinero(25.0))
    // Lista sin objetos de Dinero.
    val sinDinero: List[Dinero] = List()

    // Se suman los dineros.
    val resultadoMuchoDinero: Dinero = acumularMonoides( muchoDinero )
    val resultadoSinDinero: Dinero = acumularMonoides( sinDinero )

    // El valor mínimo que siempre se sumará es de 1.0, en caso que la lista este vacia.
    assert( Dinero(36.0) == resultadoMuchoDinero)
    assert( Dinero(1.0) == resultadoSinDinero)
  }


}
