package br.unb.cic.tp1.mh.ast

import org.scalatest._


class TesteExpNot extends FlatSpec with Matchers {

  behavior of "a not expression"
  
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

} 
