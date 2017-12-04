package br.unb.cic.tp1.mh.ast

import br.unb.cic.tp1.mh.memoria.Ambiente
import org.scalatest._

class TesteValores extends FlatSpec with Matchers {


  "An integer value 5" should "be evaluated to 5" in {
    val val5 = ValorInteiro(5)

    val5.avaliar() should be (ValorInteiro(5))
  }

}
