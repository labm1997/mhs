package br.unb.cic.tp1.mh.ast

import org.scalatest._

import br.unb.cic.tp1.exceptions.VariavelNaoDeclaradaException

class TesteExpIfThenElse extends FlatSpec with Matchers {

  behavior of "a If then Else expression"
  
  it should " if(2>3) then false else 10" in {
    val condicao = new ExpMaiorQue(ValorInteiro(2),ValorInteiro(3))
    val verdadeira = new ValorBooleano(false)
    val falsa  = new ValorInteiro(10)
    
    val ifthenelse = new ExpIfthenElse(condicao, verdadeira, falsa)

    ifthenelse.avaliar() should be (ValorInteiro(10))
  }

} 
