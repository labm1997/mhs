package br.unb.cic.tp1.mh.ast

import br.unb.cic.tp1.mh.memoria.Ambiente
import org.scalatest._
import scala.collection.mutable.ListBuffer

class TesteAplicacaoNomeada  extends FlatSpec with Matchers {

  behavior of "a named function def inc (x) = x + 1"

  it should "be evaluated to 11 when inc 1 " in {
    val inc = new DecFuncao("inc", ListBuffer[(String,Tipo)](("x",TInt())), new ExpSoma(ExpRef("x"), ValorInteiro(1)))

    Ambiente.declararFuncao(inc)

    val app = new ExpAplicacaoNomeada("inc", ListBuffer[Expressao](new ExpSoma(ValorInteiro(5), ValorInteiro(10))))

    app.avaliar() should be (ValorInteiro(16))

  }
}
