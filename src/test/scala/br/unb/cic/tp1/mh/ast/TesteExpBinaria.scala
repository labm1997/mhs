package br.unb.cic.tp1.mh.ast

import org.scalatest._

import br.unb.cic.tp1.mh.ast._

class TesteExpand extends FlatSpec with Matchers  {


  "An integer value 5 + an integer value 10" should "be evaluated to Valor(15)" in {
    val val5  = ValorInteiro(5)
    val val10 = ValorInteiro(10)

    val soma = ExpSoma(val5, val10)

    soma.avaliar() should be (ValorInteiro(15))
  }


  "A bool value true and false " should "be evaluated to ValorBooleano(false)" in {
    val val1  = ValorBooleano(true)
    val val2 = ValorBooleano(false)

    val and = ExpAnd(val1, val2)

    and.avaliar() should be (ValorBooleano(false))
  }

  "A bool value true and true " should "be evaluated to ValorBooleano(true)" in {
    val val1  = ValorBooleano(true)
    val val2 = ValorBooleano(true)

    val and = ExpAnd(val1, val2)

    and.avaliar() should be (ValorBooleano(true))
  }

  "A bool value false and true " should "be evaluated to ValorBooleano(false)" in {
    val val1  = ValorBooleano(false)
    val val2 = ValorBooleano(true)

    val and = ExpAnd(val1, val2)

    and.avaliar() should be (ValorBooleano(false))
  }

  "A bool value false and false " should "be evaluated to ValorBooleano(false)" in {
    val val1  = ValorBooleano(false)
    val val2 = ValorBooleano(false)

    val and = ExpAnd(val1, val2)

    and.avaliar() should be (ValorBooleano(false))
  }
 

}
