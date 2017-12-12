package br.unb.cic.tp1.mh.ast

import org.scalatest._
import br.unb.cic.tp1.exceptions.VariavelNaoDeclaradaException
import br.unb.cic.tp1.mh.memoria.Ambiente
import scala.collection.mutable.ListBuffer

class TesteExpAplicacaoNomeada extends FlatSpec with Matchers {

  behavior of "a funcao nomeada expression"
  
  it should "funcao(x: Int) x+1" in {
    
    val soma = new ExpSoma(new ExpRef("x"), ValorInteiro(1))
    val inc = new DecFuncao("inc", ListBuffer[(String, Tipo)](("x",TInt())), soma)
    Ambiente.declararFuncao(inc)
    
    val funcao = new ExpAplicacaoNomeada("inc", ListBuffer[Expressao](ValorInteiro(5)))
    
    funcao.avaliar should be (ValorInteiro(6))
  
  }

} 
