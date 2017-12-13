package br.unb.cic.tp1.mh.ast

import org.scalatest._

class TesteBooleanos extends FlatSpec with Matchers  {

  "A bool true and a bool false" should "be evaluated to Valor(false)" in {
    val valt  = ValorBooleano(true)
    val valf = ValorBooleano(false)

    val and = new ExpAnd(valt, valf)

    and.avaliar() should be (ValorBooleano(false))
  
  }
  
  "A bool true and a bool true" should "be evaluated to Valor(true)" in {
    val valt  = ValorBooleano(true)
    val valf = ValorBooleano(true)

    val and = new ExpAnd(valt, valf)

    and.avaliar() should be (ValorBooleano(true))
  
  }
  
  "A bool false and a bool false" should "be evaluated to Valor(false)" in {
    val valt  = ValorBooleano(false)
    val valf = ValorBooleano(false)

    val and = new ExpAnd(valt, valf)

    and.avaliar() should be (ValorBooleano(false))
  
  }
  
  "A bool false and a bool true" should "be evaluated to Valor(false)" in {
    val valt  = ValorBooleano(false)
    val valf = ValorBooleano(true)

    val and = new ExpAnd(valt, valf)

    and.avaliar() should be (ValorBooleano(false))
  
  }

  "A bool true or a bool false" should "be evaluated to Valor(true)" in {
    val valt  = ValorBooleano(true)
    val valf = ValorBooleano(false)

    val or = new ExpOr(valt, valf)

    or.avaliar() should be (ValorBooleano(true))
  
  }
  
  "A bool false or a bool false" should "be evaluated to Valor(false)" in {
    val valt  = ValorBooleano(false)
    val valf = ValorBooleano(false)

    val or = new ExpOr(valt, valf)

    or.avaliar() should be (ValorBooleano(false))
  
  }
  
  "A bool true or a bool true" should "be evaluated to Valor(true)" in {
    val valt  = ValorBooleano(true)
    val valf = ValorBooleano(true)

    val or = new ExpOr(valt, valf)

    or.avaliar() should be (ValorBooleano(true))
  
  }
  
  "A bool false or a bool true" should "be evaluated to Valor(true)" in {
    val valt  = ValorBooleano(false)
    val valf = ValorBooleano(true)

    val or = new ExpOr(valt, valf)

    or.avaliar() should be (ValorBooleano(true))
  
  }
  
  "A integer value 10 > an integer value 2" should "be evaluated to Valor(true)" in {
    val valt  = ValorInteiro(10)
    val valf = ValorInteiro(2)

    val maior = new ExpMaiorQue(valt, valf)

    maior.avaliar() should be (ValorBooleano(true))
  
  }
  
  "A integer value 10 > an integer value 20" should "be evaluated to Valor(false)" in {
    val valt  = ValorInteiro(10)
    val valf = ValorInteiro(20)

    val maior = new ExpMaiorQue(valt, valf)

    maior.avaliar() should be (ValorBooleano(false))
  
  }
  
  "A integer value 7 < an integer value 5" should "be evaluated to Valor(false)" in {
    val valt  = ValorInteiro(7)
    val valf = ValorInteiro(5)

    val menor = new ExpMenorQue(valt, valf)

    menor.avaliar() should be (ValorBooleano(false))
  
  }
  
  "A integer value 7 < an integer value 10" should "be evaluated to Valor(true)" in {
    val valt  = ValorInteiro(7)
    val valf = ValorInteiro(10)

    val menor = new ExpMenorQue(valt, valf)

    menor.avaliar() should be (ValorBooleano(true))
  
  }
  
  "A integer value 7 = an integer value 7" should "be evaluated to Valor(true)" in {
    val valt  = ValorInteiro(7)
    val valf = ValorInteiro(7)

    val igual = new ExpIgual(valt, valf)

    igual.avaliar() should be (ValorBooleano(true))
  
  }
  
  "A integer value 7 = an integer value 6" should "be evaluated to Valor(false)" in {
    val valt  = ValorInteiro(7)
    val valf = ValorInteiro(6)

    val igual = new ExpIgual(valt, valf)

    igual.avaliar() should be (ValorBooleano(false))
  
  }
  
  "A integer value 7 >= an integer value 5" should "be evaluated to Valor(true)" in {
    val valt  = ValorInteiro(7)
    val valf = ValorInteiro(5)

    val maiorigual = new ExpMaiorIgual(valt, valf)

    maiorigual.avaliar() should be (ValorBooleano(true))
  
  }
  
  "A integer value 7 >= an integer value 7" should "be evaluated to Valor(true)" in {
    val valt  = ValorInteiro(7)
    val valf = ValorInteiro(7)

    val maiorigual = new ExpMaiorIgual(valt, valf)

    maiorigual.avaliar() should be (ValorBooleano(true))
  
  }
  
  "A integer value 7 >= an integer value 10" should "be evaluated to Valor(false)" in {
    val valt  = ValorInteiro(7)
    val valf = ValorInteiro(10)

    val maiorigual = new ExpMaiorIgual(valt, valf)

    maiorigual.avaliar() should be (ValorBooleano(false))
  
  }
  
  "A integer value 15 <= an integer value 10" should "be evaluated to Valor(false)" in {
    val valt  = ValorInteiro(15)
    val valf = ValorInteiro(10)

    val menorigual = new ExpMenorIgual(valt, valf)

    menorigual.avaliar() should be (ValorBooleano(false))
  
  }
  
  "A integer value 15 <= an integer value 15" should "be evaluated to Valor(true)" in {
    val valt  = ValorInteiro(15)
    val valf = ValorInteiro(15)

    val menorigual = new ExpMenorIgual(valt, valf)

    menorigual.avaliar() should be (ValorBooleano(true))
  
  }
  
  "A integer value 15 <= an integer value 20" should "be evaluated to Valor(true)" in {
    val valt  = ValorInteiro(15)
    val valf = ValorInteiro(20)

    val menorigual = new ExpMenorIgual(valt, valf)

    menorigual.avaliar() should be (ValorBooleano(true))
  
  }

}
 
