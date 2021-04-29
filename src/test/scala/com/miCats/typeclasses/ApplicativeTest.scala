package com.miCats.typeclasses

import org.scalatest.AsyncFunSuite

class ApplicativeTest extends AsyncFunSuite {

  /*
    Applicative Functors o más fácil Applicative: Applicative[F[_]]

    * Extiende de Functor, pero a diferencia de éste, es capaz de trabajar con multiples
       efectos independientes y posteriormente componerlos.
    * La idea es que todos los efectos sean evaluados independientemente del resultado del cada uno.
    *   Si la evaluación de un efecto falla, no afectará la evaluación de los demás.

    * Tiene una función `pure`, que permite embolver un dato dentro de un type constructor =>  def pure[A](x: A): F[A]

   */

  test("Podemos crear un Applicative de Option dando simplemente " +
    "el valor interno que tendrá el Option. En otras palabas aplicar un valor a un efecto.") {
    import cats.Applicative
    import cats.implicits._

    val valorOpcional: Option[Int] = Applicative[Option].pure(1)
    assertResult( Some(1 )) {
      valorOpcional
    }
  }

  test("Los applicatives nos permiten aplicar una valor a un efecto, y a su vez poder aplicarlo " +
    "a una función también en un efecto."){
    import cats.Applicative
    import cats.implicits._

    def multiplicarEnTexto(numero1: Int)(numero2: Long): String = s"${(numero1 * numero2.toInt)}"

    val posibleNumero: Option[Int] = Some(3)
    /*
    Al llamar la función de `multiplicarEnTexto` en el map, automáticamente se pasa como primer parámetro
    el Int que esta dentro del Option. Quedando la función parcialmente aplicada, necesitando un Long
    para poder entregar el String resultante. Lo anterior envuelto en un Option.
     */
    val funcionDentroEfecto: Option[Long => String] = posibleNumero.map(multiplicarEnTexto)

    val miAplicative = Applicative[Option]

    assertResult(Some("15")) {
      /*
      Con ap podemos aplicar a una función un valor, ambos dentro de un contexto
      (para el caso un Option)
       */
      miAplicative.ap(funcionDentroEfecto)(miAplicative.pure(5L))
    }
  }

  test("No solo podemos aplicar un valor a una función, sino también aplicar multiples valores " +
    "a una función. Acá aplicaremos 5 valores a una misma función") {

    import cats.Applicative
    import cats.implicits._

    import scala.concurrent.Future

    def obtenerUsuario(): Future[String] = Future("Pedro")
    def obtenerDatos(): Future[String] = Future("Programador")
    def obtenerEdad(): Future[Int] = Future(29)

    case class Persona(usuario: String, datos: String, edad: Int)

    val miApplicativeFuture = Applicative[Future]

    val funcionConEfecto = miApplicativeFuture.pure(Persona(_,_,_))
    miApplicativeFuture.ap3(funcionConEfecto)(obtenerUsuario(), obtenerDatos(), obtenerEdad())
      .map( per => assert( per.edad == 29 ) )
  }


  test("Los Applicatives también se pueden componer... ") {
    import cats.Applicative
    import cats.implicits._

    import scala.concurrent.Future

    type PosiblesValoresOpcionales[A] = Future[List[Option[A]]]

    implicit val composeApplicatives = Applicative[Future] compose Applicative[List] compose Applicative[Option]

    val primerValor: PosiblesValoresOpcionales[Int] = Future(List(None, Some(10)))
    val segundoValor: PosiblesValoresOpcionales[Int] = Future(List(Some(20), None))
    val tercerValor: PosiblesValoresOpcionales[Int] = Future(List(Some(5), Some(5)))

    val resultado: PosiblesValoresOpcionales[Int] = Applicative[PosiblesValoresOpcionales](composeApplicatives)
      .map3( primerValor, segundoValor, tercerValor )((diez, veinte, cinco) => diez + veinte + cinco)
    println(resultado)
    // TODO: Por qué da este resutlado ???
    resultado.map( algo =>
      assert(algo == List(None, None, None, None, Some(35), Some(35), None, None))
    )
  }

  test("Los applicative son capaz de trabajar con multiples efectos. " +
    "el for-comprehension es fail fast, lo que quiere decir que al 'fallar' uno de las instrucciones " +
    "que están dentro del for, harán que las siguientes instrucciones no se ejecuten. Esto es un " +
    "problema cuando queremos acumular todos los errores que están dentro del for." +
    "Con los Applicatives podemos hacer acumulación de errores. ") {

    import cats.data.Validated.{Invalid, Valid, _}
    import cats.data._
    import cats.implicits._

    // MODELO
    case class Usuario(nombre: String, edad: Int, email: String, genero: Char)

    // Alias para Validated[NonEmptyChain[E], A]
    type DatoValidado[A] = ValidatedNec[ErrorNegocio, A]

    sealed trait ErrorNegocio {
      def mensajeDeError: String
    }

    case object MenorEdad extends ErrorNegocio {
      override def mensajeDeError: String = "Es menor de edad"
    }

    case object EmailInvalido extends ErrorNegocio {
      override def mensajeDeError: String = "El correo no es correcto"
    }

    case object GeneroInexistente extends ErrorNegocio {
      override def mensajeDeError: String = "El genero no existe"
    }

    // VALIDACIONES DE NEGOCIO.
    def validarMayoriaEdad(edad: Int): DatoValidado[Int] = {
      if ( edad >= 18 )
      edad.validNec
      else
      MenorEdad.invalidNec
    }

    def validarEmail(email: String): DatoValidado[String] = {
      if( email.contains("@"))
      email.validNec
      else
      EmailInvalido.invalidNec
    }

    def validarGenero(char: Char): DatoValidado[Char] = {
      if(char.equals('M') || char.equals('F'))
      char.validNec
      else
      GeneroInexistente.invalidNec
    }

    // OBJETO A VALIDAR.
    val miUsarioConErrores = Usuario("Juan", 10, "invalido",'M')

    /*
    Con ayuda del `mapN` podemos hacer multiples validaciones y
    acumular los errores.
     */
    val validacionFallida = (
      validarEmail(miUsarioConErrores.email),
      validarMayoriaEdad(miUsarioConErrores.edad),
      validarGenero(miUsarioConErrores.genero)
    ).mapN((mail, edad, genero) => {
      Usuario(miUsarioConErrores.nombre, edad, mail, genero)
    } )

    /*
    Las validaciones de Mayoría de edad y Email valido fallaron,
    ya que no cumplía con las condiciones, pero se logran obtener
    todos los errores en una sola validación.
     */
    validacionFallida match {
      case Valid( _ ) => assert(false)
      case Invalid( errores ) => {
        assert( errores.toList.contains(MenorEdad) )
        assert( errores.toList.contains(EmailInvalido))
      }
    }

    // OBJETO A VALIDAR.
    val miUsarioCorrecto = Usuario("Juan", 19, "correo@gmail.com",'M')

    /*
    Con ayuda del `mapN` podemos hacer multiples validaciones y
    acumular los errores.
     */
    val validacionExitosa = (
      validarEmail(miUsarioCorrecto.email),
      validarMayoriaEdad(miUsarioCorrecto.edad),
      validarGenero(miUsarioCorrecto.genero)
    ).mapN((mail, edad, genero) => {
      Usuario(miUsarioCorrecto.nombre, edad, mail, genero)
    } )

    /*
    Todas las validaciones son exitosas, así que podemos obtener
    una respuesta positiva.
     */
    validacionExitosa match {
      case Valid( usuarioValidado ) => assert(usuarioValidado.edad == 19 )
      case Invalid( _ ) => assert( false )
    }
  }

}
