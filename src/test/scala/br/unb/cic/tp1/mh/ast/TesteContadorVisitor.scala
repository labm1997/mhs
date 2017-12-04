package br.unb.cic.tp1.mh.ast

import br.unb.cic.tp1.mh.visitors.ContadorVisitor

import br.unb.cic.tp1.mh.ast.ExpSoma
import org.scalatest._

class TesteContadorVisitor extends  FlatSpec with Matchers{

  behavior of "an application of our visitor"

  it should "be evaluated to 5 when (3+4)+5" in {
    val soma = ExpSoma(ExpSoma(ValorInteiro(3), ValorInteiro(4)),
                                ValorInteiro(5))

    val c = new ContadorVisitor()

    soma.aceitar(c)

    c.contador should be (5)
  }

}
