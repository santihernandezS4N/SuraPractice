package com.miCats.typeclasses

import cats.Id
import org.scalatest.FunSuite

class ReaderMonadTest extends FunSuite {

  /*
    Reader nos permite crear operaciones o cumputos que dependen de alguna entrada.
    Un uso común para los Reader es la inyección de dependencias.
    Un Reader representa una función A => B.
    Los Reader se deberían usar cuando:
      * Queremos testear partes aisladas de un programa
      * Queremos diferenciar los parametrós de entrada de las dependencias
   */

  test("Creamos una función que necesita una Operacion para poder ejecutarse. " +
    "Dado:" +
    " - Dos funciones que suma y restan dos valores de una operacion. Pero la operación no se pasa " +
    "     como parámetro de las funciones, sino que entra como 'inyeción de dependencias'" +
    " - Una operación con dos números." +
    "Entonces: " +
    " - Al llamar la función de suma o la de resta no se envía la operación, pero al llamar la función " +
    "     `run` nos pide la operación, para poder obtener el valor. " +
    " - Al llamar las dos funciones dentro de un for-comprehension solo es necesario llamar " +
    "     la función `run` una sola vez. ") {
    import cats.data.Reader

    case class Operacion(numero1: Int, numero2: Int)

    //Esta función necesita un Operacion para poder efectuar la suma que tiene dentro de la función
    def redSuma(): Reader[Operacion, Int] = Reader {
      case operacion: Operacion =>
        operacion.numero1 + operacion.numero2
    }

    def redResta(): Reader[Operacion, Int] = Reader {
      case operacion: Operacion =>
        operacion.numero1 - operacion.numero2
    }

    val operacion = Operacion(4, 3)
    // Es necesario llamar la función run y parsarle "lo que necesita" para
    // poder ejecutar el computo interno.
    val resultadoSuma: Int = redSuma().run(operacion)
    assert( resultadoSuma == 7 )
    val resultadoResta: Int = redResta().run(operacion)
    assert( resultadoResta == 1)

    // También podemos lograr lo mismo usando un for-comprehension
    val resultadoTuplaFor = for {
      resSuma <- redSuma()
      resResta <- redResta()
    } yield {
      (resSuma, resResta)
    }
    // Al final solo pasaremos una sola vez la operación
    val resultadoFor: (Int, Int) = resultadoTuplaFor.run(operacion)

    assert( resultadoFor == (7, 1))
  }

  test("Calcular el iva de un producto pasándo a través de un Reader la configuración del sistema. " +
    "Donde: " +
    " - Se carga la configuración del sistema. " +
    " - Se define una función para convertir un String a un Option[Double]. None en caso de no poderse convertir. " +
    " - Se define una función para calcular el iva de un producto, tomando el valor del producto y multiplicandolo " +
    "     por el iva (el cuál es obtenido de la configuración del sistema.). " +
    " - Se crea un producto. " +
    " - Se envía el producto a la función de calcular el precio con iva incluido. " +
    " - Se hace validación de la respuesta del calculo del iva + valor del producto. " +
    "Entonces: " +
    " - Al valor del producto se le añade el iva obtenido de la configuración. ") {
    import cats.data.Reader
    import com.typesafe.config.{Config, ConfigFactory}

    val config = ConfigFactory.load()

    case class Producto(nombre:String, valor:Double)

    def aDouble(s: String ): Option[Double] = {
      try
        Some( s.toDouble )
      catch {
        case _: NumberFormatException => None
      }
    }

    def calcularProductoConIva(producto: Producto): Reader[Config, Either[String, Double]] = Reader {
      case config: Config =>
        val opValorIva: Option[Double] = aDouble(config.getString("impuestos.iva"))

        opValorIva.fold[Either[String, Double]](
          Left("No se encontró el valor del iva")
        )(
          iva => Right(producto.valor * iva)
        )
    }

    val computador = Producto( "portatil", 100 )
    // Se llama la función `run` para enviar o pasar la configuración del sistema
    val valorIva: Id[Either[String, Double]] = calcularProductoConIva(computador).run(config)

    valorIva match {
      case Right( valor ) => assert( valor == 119D)
      case Left( _ ) => assert( false )
    }
  }

}
