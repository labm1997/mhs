 
package br.unb.cic.tp1.mh.ast

import org.scalatest._

class TesteInteiros extends FlatSpec with Matchers  {


  "An integer value 5 + an integer value 10" should "be evaluated to Valor(15)" in {
    val val5  = ValorInteiro(5)
    val val10 = ValorInteiro(10)

    val soma = new ExpSoma(val5, val10)

    soma.avaliar() should be (ValorInteiro(15))
  
  }
  
  "An integer expression (5*10) + an integer value 10" should "be evaluated to Valor(60)" in {
    val val5  = new ExpMult(ValorInteiro(5),ValorInteiro(10))
    val val10 = ValorInteiro(10)

    val soma = new ExpSoma(val5, val10)

    soma.avaliar() should be (ValorInteiro(60))
  
  }

  "An integer value 5 - an integer value 10" should "be evaluated to Valor(-5)" in {
    val val5  = ValorInteiro(5)
    val val10 = ValorInteiro(10)

    val sub = new ExpSub(val5, val10)

    sub.avaliar() should be (ValorInteiro(-5))
  
  }
  
  "An integer expression (10/10) - an integer value 10" should "be evaluated to Valor(-9)" in {
    val val5  = new ExpDiv(ValorInteiro(10),ValorInteiro(10))
    val val10 = ValorInteiro(10)

    val sub = new ExpSub(val5, val10)

    sub.avaliar() should be (ValorInteiro(-9))
  
  }

  "An integer value 5 * an integer value 10" should "be evaluated to Valor(50)" in {
    val val5  = ValorInteiro(5)
    val val10 = ValorInteiro(10)

    val mult = new ExpMult(val5, val10)

    mult.avaliar() should be (ValorInteiro(50))
  
  }
  
  "An integer expression (10+400) * an integer value 10" should "be evaluated to Valor(4100)" in {
    val val5  = new ExpSoma(ValorInteiro(10),ValorInteiro(400))
    val val10 = ValorInteiro(10)

    val mult = new ExpMult(val5, val10)

    mult.avaliar() should be (ValorInteiro(4100))
  
  }

  "An integer value 10 / an integer value 2" should "be evaluated to Valor(5)" in {
    val val10  = ValorInteiro(10)
    val val2 = ValorInteiro(2)

    val div = new ExpDiv(val10, val2)

    div.avaliar() should be (ValorInteiro(5))
  
  }
  
  "An integer expression (10-2) / an integer value 2" should "be evaluated to Valor(4)" in {
    val val10  = new ExpSub(ValorInteiro(10), ValorInteiro(2))
    val val2 = ValorInteiro(2)

    val div = new ExpDiv(val10, val2)

    div.avaliar() should be (ValorInteiro(4))
  
  }
}
