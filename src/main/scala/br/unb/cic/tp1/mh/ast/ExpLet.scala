package br.unb.cic.tp1.mh.ast

import br.unb.cic.tp1.mh.memoria.Ambiente
import br.unb.cic.tp1.mh.tc.Gamma
import br.unb.cic.tp1.mh.visitors.Visitor


class ExpLet(val id : String, val expNomeada : Expressao, val corpo : Expressao) extends Expressao {
override def avaliar(): Valor = {
    val valor = expNomeada.avaliar() // innermost strategy
    Ambiente.atualiza(id, valor)
    //println("vou avaliar o corpo: " + corpo)
    val ret = corpo.avaliar()
    //println("vou retirar: " + id)
    Ambiente.retirar(id)
    return ret
  }

  override def verificaTipo: Tipo = {
    if(expNomeada.verificaTipo == TErro()) {
      return TErro()
    }
    Gamma.mapear(id, expNomeada.verificaTipo)
    return corpo.verificaTipo
  }

  override def aceitar(v: Visitor): Unit = {
    v.visitar(this)
  }
}
