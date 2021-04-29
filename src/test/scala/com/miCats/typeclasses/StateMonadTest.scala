package com.miCats.typeclasses

import org.scalatest.FunSuite

class StateMonadTest extends FunSuite {

  /*
    State Monad se usa para pasar un estado como parte de un computo.
    Con State Monad podemos modelar mutabilidad en la programación funcional pura, sin usar mutabilidad.

   */

  test("Se crea un State que dado un número lo multiplica por 5 y devuelve su valor y un String " +
    "indicando cuándo dió la multiplicación. ") {
    import cats.data.State

    val por5 = State[Int, String] { numero =>
      val resultado = numero * 5
      (resultado, s"${numero} multiplicado por 5 es ${resultado}")
    }

    val numeroAMultiplicar = 7
    /*
    La función `run` permite pasar un estado inicial, además que es la función que nos permite
    obtener tanto estado final "trasnformado" (por no llamarlo mutado) y el valor final.
     */
    assert( por5.run(numeroAMultiplicar).value == (35,"7 multiplicado por 5 es 35"))
    /*
    Con la función `runS` obtenemos el valor del estado final
     */
    assert( por5.runS(numeroAMultiplicar).value == 35 )
    /*
    Con la función `runA` obtenemos el valor final
     */
    assert( por5.runA(numeroAMultiplicar).value == "7 multiplicado por 5 es 35" )

  }

  test("Con State Monad calculamos el valor final de un producto. " +
    "Dado: " +
    " - Una función que al valor del producto le suma la ganancia. " +
    " - Una función que al valor del producto le suma el iva. " +
    " - Un for comprehension que llama las dos funciones anteriores. " +
    " - Un producto que vale 100. " +
    " - Se envía le valor del producto al resultado del for comprehension para " +
    "     calcular el valor total del producto. " +
    " Entonces: " +
    " - La sumatoria es secuencia, el estado de la primera función es modificado en " +
    "     la segunda función. ") {
    import cats.data.State

    case class Producto(nombre:String, valorBruto: Double)

    // porcentaje de ganancia de los productos.
    val ganancia = 1.19
    // porcentaje del iva que se le aplica a los productos.
    val iva = 1.15
    // Se define un producto con un valor
    val computador = Producto("portatil", 100)

    def calcularValorGanancia(): State[Double, String] = State {
      valor =>
        val resultado = valor * ganancia
        (resultado, s"Valor con ganancia es: ${resultado} ")
    }

    def calcularValorIva(): State[Double, String] = State {
      valor =>
        val resultado = valor * iva
        (resultado, s"Valor con iva es: ${resultado}")
    }

    // Se hace el llamado secuencial de ambas funciones
    val resultado = for {
      a <- calcularValorGanancia()
      b <- calcularValorIva()
    } yield {
      a + b
    }

    /*
    El valor bruto del producto más el valor de la ganancia será el "estado"
    que posteriormente se modificará en la segunda función.

    Si se pasara el valor del proucto a cada función y luego se sumara daría el siguiente resultado:
    (100 * 1.19)+(100*1.15) = 119.0 + 115.0 = 234

    Diferente al que realmente obtenemos, que es 136.85, igual a sumarle el porcejantaje de la ganancia,
    y a ese sumarle a su vez el valor del iva.

     */
    assert(
      resultado.run(computador.valorBruto).value == (136.85, "Valor con ganancia es: 119.0 Valor con iva es: 136.85")
    )

  }

}
