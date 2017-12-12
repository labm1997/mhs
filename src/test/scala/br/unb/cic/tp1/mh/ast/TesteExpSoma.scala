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

  "A bool true or a bool false" should "be evaluated to Valor(true)" in {
    val valt  = ValorBooleano(true)
    val valf = ValorBooleano(false)

    val or = new ExpOr(valt, valf)

    or.avaliar() should be (ValorBooleano(true))
  
  }

  "A bool !true" should "be evaluated to Valor(false)" in {
    val valt  = ValorBooleano(true)

    val not = new ExpNot(valt)

    not.avaliar() should be (ValorBooleano(false))
  
  }

  "A bool !false" should "be evaluated to Valor(true)" in {
    val valt  = ValorBooleano(false)

    val not = new ExpNot(valt)

    not.avaliar() should be (ValorBooleano(true))
  
  }

  "A bool !false" should "have type TBool" in {
    val valt  = ValorBooleano(false)

    val not = new ExpNot(valt)

    not.verificaTipo should be (TBool())
  
  }
  
  "A integer value 10 > an integer value 2" should "be evaluated to Valor(true)" in {
    val valt  = ValorInteiro(10)
    val valf = ValorInteiro(2)

    val maior = new ExpMaiorQue(valt, valf)

    maior.avaliar() should be (ValorBooleano(true))
  
  }
  
  "A integer value 7 < an integer value 5" should "be evaluated to Valor(false)" in {
    val valt  = ValorInteiro(7)
    val valf = ValorInteiro(5)

    val menor = new ExpMenorQue(valt, valf)

    menor.avaliar() should be (ValorBooleano(false))
  
  }
  
  "A integer value 7 = an integer value 7" should "be evaluated to Valor(true)" in {
    val valt  = ValorInteiro(7)
    val valf = ValorInteiro(7)

    val igual = new ExpIgual(valt, valf)

    igual.avaliar() should be (ValorBooleano(true))
  
  }
  
  "A integer value 7 >= an integer value 5" should "be evaluated to Valor(true)" in {
    val valt  = ValorInteiro(7)
    val valf = ValorInteiro(5)

    val maiorigual = new ExpMaiorIgual(valt, valf)

    maiorigual.avaliar() should be (ValorBooleano(true))
  
  }
  
  "A integer value 15 <= an integer value 10" should "be evaluated to Valor(false)" in {
    val valt  = ValorInteiro(15)
    val valf = ValorInteiro(10)

    val menorigual = new ExpMenorIgual(valt, valf)

    menorigual.avaliar() should be (ValorBooleano(false))
  
  }

}
