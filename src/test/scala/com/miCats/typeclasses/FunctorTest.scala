package com.miCats.typeclasses

import org.scalatest.AsyncFunSuite

class FunctorTest extends AsyncFunSuite {

  /*
  Básicamente un Fuctor es cualquier cosa que tenga la función `map`.

  Un funtor F[A]:
   * Es un type class
   * Tiene una función clave llamada map => def map[A, B](fa: F[A])(f: A => B): F[B]
      - (fa: F[A]) es un tipo de dato envuelto en un Functor
      - (f: A => B) es una función que transforma de un tipo A a un tipo B. Convertir un String a Int.
      - F[B] Es el resultado final envuelto en un functor.

    * Las leyes que se deben cumplir para la función map son:
        - Identity: Cuando se llama la función map sobre un Functor, éste devuelve el mismo Fuctor.
            Esto garantiza que si se aplica una función sobre el map, este no hará ninguna otra operación
            sobre el valor del Functor.
        - Composition: hacer map sobre las funciones g y f es lo mismo que hacer map sobre g y luego map sobre f
            fa.map(g(f(_))) == fa.map(f).map(g)
   */

  test("Option es un functor, con el cual demostramos que cumple la ley de Identity") {
    val valorOpcional: Option[Int] = Some(10)
    assert( Some(10) == valorOpcional.map( identity ))
  }

  test("Future es un functor, con el cual demostramos que cumple la ley de Composition") {
    import scala.concurrent.Future

    val futuro:Future[String] = Future("Cats")

    def mayusculas(valor: String): String = {
      valor.toUpperCase
    }

    def saludar(valor: String): String = {
      s"Hola ${valor}"
    }

    val saludo1: Future[String] = futuro.map(valor => saludar(mayusculas(valor)))
    val saludo2: Future[String] = futuro.map(valor => mayusculas(valor)).map( valor => saludar(valor))

    for {
      s1 <- saludo1
      s2 <- saludo2
    } yield {
      assert( s1 == "Hola CATS")
      assert( s1 == s2)
    }
  }

  test("Componiendo diferentes Functores. " +
    "Para componer diferentes functores se usa la función `compose` y así evitamos tener que estar " +
    "usando repetidamente la función `map`") {

    import cats.Functor
    import cats.implicits._

    import scala.concurrent.Future

    val dato: Future[Option[Int]] = Future(Some(5))

    def doblarNumero(numero: Int): Int = numero * 2

    /*
    Al llamar la función `map` se reciben dos parámetros:
      1. (fa: Future[Option[A]) un tipo de dato que cumpla con F[G[_]]
      2. (f: A => B) la función que parsará del tipo de dato A a B
     */
    val futuroCombinacion: Future[Option[Int]] = Functor[Future].compose[Option].map(dato)(doblarNumero)

    /*
    Sin hacer composición la manera para aplicar la función al tipo de dato Int sería
    hacer map sobre el Futuro, posteriormente map sobre el Option, y en ese momento
    aplicar la función sobre el valor del Option.

      dato.map(_.map(doblarNumero))

     */
    futuroCombinacion.map( valor => assert( Some(10) == valor))
  }

  test("Componiendo diferentes Functores. " +
    "Componemos 3 Fuctores diferentes, `Future`, `List` y `Option`, a través de la función `compose`. " +
    "Permitiendo acceder al valor interno sin necesidad de anidar llamados a la función `map`. ") {

    import cats.Functor
    import cats.implicits._

    import scala.concurrent.Future

    // Se crea un tipo propio de dato, que represente algo para nosotros.
    type PosiblesElementosAsync[x] = Future[List[Option[x]]]

    // implicito requerido para identificar cómo componer los Functores.
    implicit val implicitPosiblesElementosAsync = Functor[Future] compose Functor[List] compose Functor[Option]

    // Funciones de negocio
    def funcionDoblarNumero(numero: Int): Int = numero * 2
    def funcionAMayusculas(texto: String): String = texto.toUpperCase

    // Tenemos dos PosiblesElementosAsync de enteros y palabras, simulando el resultado de una consulta a BD
    val miFloInt: PosiblesElementosAsync[Int] = Future(List(Some(5), None))
    val miFloString: PosiblesElementosAsync[String] = Future(List(Some("hola"), Some("Functor")))

    // Simplemente indicamos que vamos a hacer un `map` a nuestro tipo de dato (que son functores anidados)
    // y solo es necesario pasar cuál es la transformación que se quiere hacer.
    val resultadoCombinacionInt: PosiblesElementosAsync[Int] = Functor[PosiblesElementosAsync](implicitPosiblesElementosAsync)
      .map(miFloInt)(funcionDoblarNumero)
    val resultadoCombinacionString: PosiblesElementosAsync[String] = Functor[PosiblesElementosAsync](implicitPosiblesElementosAsync)
      .map(miFloString)(funcionAMayusculas)
    // Con un solo llamado a la función `map` pudimos aplicar una función al tipo de dato de nuestro propio tipo,
    // siendo mucho más fácil de leer que llamados anidados de `map`.


    // Finalmente se valida que las transformaciones fueron aplicadas.
    resultadoCombinacionInt map {
      x => assert( x == List(Some(10), None) )
    }
    resultadoCombinacionString map {
      x => assert( x == List(Some("HOLA"), Some("FUNCTOR")))
    }
  }


  /*
    Type constructor:
    * Son como tipos de tipos. Se diferencian de los tipos (type) normales porque tienen un "hole" (_).
    * Option es un type constructor porque tiene un "hole" Option[_], donde podemos reemplazar el _ por el
        tipo que queremos, ejemplo: Option[Int], Option[Persona].
    *Se debe diferenciar los type de los type constructor
        List    => type constructor
        List[A] => type
        Se puede hacer el simil con las funciones y los valores. Una función necesita parámetros para dar un resultado
        Mientras que el valor ya es el resultado en si. Los Type Constructor necesitan un "parámetro"
   */

}
