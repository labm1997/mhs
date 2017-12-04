package br.unb.cic.tp1.mh.ast

import br.unb.cic.tp1.exceptions.VariavelNaoDeclaradaException
import br.unb.cic.tp1.mh.memoria.Ambiente
import br.unb.cic.tp1.mh.tc.Gamma
import br.unb.cic.tp1.mh.visitors.Visitor

case class ExpRef(variavel : String) extends Expressao {

  override def avaliar(): Valor = {
    try {
      return Ambiente.consulta(variavel)
    }
    catch {
      case ex: NoSuchElementException => throw VariavelNaoDeclaradaException()
    }
  }

  override def verificaTipo: Tipo = Gamma.consultar(variavel)

  override def aceitar(v: Visitor): Unit = {
    v.visitar(this)
  }
}
