package br.unb.cic.tp1.mh.ast

import org.scalatest._

import br.unb.cic.tp1.exceptions.VariavelNaoDeclaradaException
import br.unb.cic.tp1.mh.ast.ExpSoma

class TesteExpLambda extends FlatSpec with Matchers {

  behavior of "a lambda expression"
  
  it should "be evaluated to Closure(x, x+1) when (x) -> x + 1" in {
    val inc  = new ExpLambda("x", TInt(),
      new ExpSoma(new ExpRef("x"), ValorInteiro(1)))


    val closure = inc.avaliar().asInstanceOf[Closure]
    closure.id should be ("x")
    closure.corpo should be (new ExpSoma(new ExpRef("x"), ValorInteiro(1)))
  }

}
