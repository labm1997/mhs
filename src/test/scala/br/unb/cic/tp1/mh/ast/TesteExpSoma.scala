package br.unb.cic.tp1.mh.ast

import org.scalatest._

class TesteExpSoma extends FlatSpec with Matchers  {


  "An integer value 5 + an integer value 10" should "be evaluated to Valor(15)" in {
    val val5  = ValorInteiro(5)
    val val10 = ValorInteiro(10)

    val soma = new ExpSoma(val5, val10)

    soma.avaliar() should be (ValorInteiro(15))
  
  }

  "An integer value 5 - an integer value 10" should "be evaluated to Valor(-5)" in {
    val val5  = ValorInteiro(5)
    val val10 = ValorInteiro(10)

    val sub = new ExpSub(val5, val10)

    sub.avaliar() should be (ValorInteiro(-5))
  
  }

  "An integer value 5 * an integer value 10" should "be evaluated to Valor(50)" in {
    val val5  = ValorInteiro(5)
    val val10 = ValorInteiro(10)

    val mult = new ExpMult(val5, val10)

    mult.avaliar() should be (ValorInteiro(50))
  
  }

  "An integer value 10 / an integer value 2" should "be evaluated to Valor(5)" in {
    val val10  = ValorInteiro(10)
    val val2 = ValorInteiro(2)

    val div = new ExpDiv(val10, val2)

    div.avaliar() should be (ValorInteiro(5))
  
  }

  "A bool true and a bool false" should "be evaluated to Valor(false)" in {
    val valt  = ValorBooleano(true)
    val valf = ValorBooleano(false)

    val and = new ExpAnd(valt, valf)

    and.avaliar() should be (ValorBooleano(false))
  
  }

 

}
