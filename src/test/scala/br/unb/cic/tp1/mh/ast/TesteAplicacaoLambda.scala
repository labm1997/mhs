package br.unb.cic.tp1.mh.ast

import br.unb.cic.tp1.mh.memoria.Ambiente
import org.scalatest._

class TesteAplicacaoLambda extends FlatSpec with Matchers {

  behavior of "an application of a lambda expression"
  
  it should "be evaluated to Valor(6) when ((x) -> x + 1) 5)" in {
    Ambiente.iniciar()
    val inc = new ExpLambda("x", TInt(), new ExpSoma(new ExpRef("x"), ValorInteiro(1)))
    val app = new ExpAplicacaoLambda(inc, ValorInteiro(5)) 

    app.avaliar() should be (ValorInteiro(6))
  }

  it should "be evaluated to 20 when let y = 10 in let f = (x -> x +y) in let y = 20 in f 10" in {
    Ambiente.iniciar()
    val let1 = new ExpLet("y", ValorInteiro(20), ExpAplicacaoLambda(ExpRef("f"), ValorInteiro(10)))
    val let2 = new ExpLet("f", new ExpLambda("x", TInt(), new ExpSoma(ExpRef("x"), ExpRef("y"))), let1)
    val let3 = new ExpLet("y", ValorInteiro(10), let2)

    let3.avaliar() should be (ValorInteiro(20))
  }

}
