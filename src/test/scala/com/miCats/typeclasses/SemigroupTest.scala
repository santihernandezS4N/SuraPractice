package com.miCats.typeclasses

import org.scalatest.FunSuite

class SemigroupTest extends FunSuite{

  /*
    Un Semigroup es un type class que tiene una operación asociativa y se expresa así:

    trait Semigroup[A] {
      def combine(x: A, y: A): A
    }

   */

  test("Mi primer Semigroup. Un Semigroup permite combinar dos elementos del mismo " +
    "tipo, dando como resultado solo un elemento de ese tipo. Adicionalmente no debe verse alterado " +
    "el resultado si se cambia el orden de la combinación. (A esto se le llama propiedad asociativa) " +
    "Dado: " +
    " -> Se crea un implicit Semigroup para Int, redefiniendo la operación asociativa. " +
    " -> Se definen 3 números" +
    " -> Se hacen 2 combinaciones de dos números, alterando el orden en una de las dos veces." +
    " -> Se hacen 2 combinaciones de tres números, alterando el orden en una de las dos veces. " +
    "Entonces: " +
    " -> El resultado de la combinación de los 2 números es igual. " +
    " -> El resultado de la combinación de los 3 números es igual. ") {

    import cats.Semigroup

    implicit val intCombinador: Semigroup[Int] = new Semigroup[Int] {
      override def combine(x: Int, y: Int): Int = x + y
    }

    val numero1 = 2
    val numero2 = 5
    val numero3 = 7

    // Validamos que la propiedad asociativa si se cumpla.
    // (el orden de los elementos en la combinación no altera el resultado en sí )
    val resultadoCombinacion1 = Semigroup[Int].combine( numero1, numero2)
    val resultadoCombinacion2 = Semigroup[Int].combine( numero2, numero1)
    assert( resultadoCombinacion1 == 7)
    assert( resultadoCombinacion2 == 7)

    // Validamos que la propiedad asociativa si se cumpla.
    // (el orden de los elementos en la combinación no altera el resultado en sí )
    val resultadoCombinacion3 = Semigroup[Int].combine( numero1, Semigroup[Int].combine(numero2, numero3))
    val resultadoCombinacion4 = Semigroup[Int].combine( numero3, Semigroup[Int].combine(numero2, numero1))
    assert( resultadoCombinacion3 == 14)
    assert( resultadoCombinacion4 == 14)
  }

  test("Se usa la sintaxis Infix para los tipos que tengan instancia de Semigroup. " +
    "Dado: " +
    " -> Se importa la sintaxis Infix" +
    " -> Se crea un implicit Semigroup para Int, definiendo la operación de combinar. " +
    " -> Se definen 3 números" +
    " -> Se hacen 2 combinaciones de dos números, alterando el orden en una de las dos veces." +
    " -> Se hacen 2 combinaciones de tres números, alterando el orden en una de las dos veces. " +
    "Entonces: " +
    " -> El resultado de la combinación de los 2 números es igual. " +
    " -> El resultado de la combinación de los 3 números es igual. ") {

    import cats.Semigroup
    import cats.syntax.semigroup._

    implicit val intCombinador: Semigroup[Int] = new Semigroup[Int] {
      override def combine(x: Int, y: Int): Int = x + y
    }

    val numero1 = 2
    val numero2 = 5
    val numero3 = 7

    val resultadoCombinacion1 = numero1 |+| numero2
    val resultadoCombinacion2 = numero2 |+| numero1
    val resultadoCombinacion3 = numero1 |+| numero2 |+| numero3
    val resultadoCombinacion4 = numero3 |+| numero2 |+| numero1

    assert( resultadoCombinacion1 == 7)
    assert( resultadoCombinacion2 == 7)
    assert( resultadoCombinacion3 == 14)
    assert( resultadoCombinacion4 == 14)
  }

  test("Se crea un Semigroup para un tipo propio. " +
    "Dado: " +
    " -> Se importa la sintaxis Infix. " +
    " -> Se crea un tipo Botella que contiene una cantidad de liquido. " +
    " -> Se crea un implicit Semigroup para Botella, definiendo la operación de combinar. " +
    " -> Se definen 2 botellas con diferente cantidad de liquido interno. " +
    " -> Se combinan las dos botellas. " +
    "Entonces: " +
    " -> El resultado de la combinación da una botella con la cantidad igual a la suma de ambas cantidades internas. ") {
    import cats.Semigroup
    import cats.syntax.semigroup._

    case class Botella( cantidadLiquido: Int )

    implicit val botellaCombinador: Semigroup[Botella] = new Semigroup[Botella] {
      override def combine(x: Botella, y: Botella): Botella = Botella( x.cantidadLiquido + y.cantidadLiquido )
    }

    val aguaPequenia = Botella( 2 )
    val aguaGrande = Botella( 5 )

    val botellaAguaResultante = aguaPequenia |+| aguaGrande

    assert( botellaAguaResultante == Botella( 7 ) )
  }

  test("Se crea un Semigroup para un tipo propio. " +
    "Dado: " +
    " -> Se crea un tipo Papel con dos atributos de ancho y alto. " +
    " -> Se crea un Semigroup para el tipo Papel. " +
    " -> Se crea una lista con papeles de varios tamaños. " +
    " -> Se suman los papeles de izquierda a derecha y de derecha a izquierda." +
    "Entonces: " +
    " -> Ambas sumas de los papeles debe dar el mismo tamaño. " ) {
    import cats.Semigroup
    import cats.syntax.semigroup._

    case class Papel(ancho: Double, alto: Double)

    implicit val combinarPapel: Semigroup[Papel] = new Semigroup[Papel] {
      override def combine(x: Papel, y: Papel): Papel = Papel(
        x.ancho + y.ancho,
        if (x.alto > y.alto ) x.alto else y.alto
      )
    }

    val muchosPapeles: List[Papel] = List(Papel(2.0, 1.0), Papel(3.5, 1.3), Papel(1.8, 1.9), Papel(1.0,1.0))

    val resultadoPapelesDerecha: Papel = muchosPapeles.foldRight(Papel(0.0,0.0))( _ |+| _ )
    val resultadoPapelesIzquierda: Papel = muchosPapeles.foldLeft(Papel(0.0,0.0))( _ |+| _ )

    assert( resultadoPapelesDerecha == Papel(8.3, 1.9))
    assert( resultadoPapelesIzquierda == Papel(8.3, 1.9))
  }

}
