package br.unb.cic.tp1.mh.ast

import br.unb.cic.tp1.mh.memoria.Ambiente
import br.unb.cic.tp1.mh.tc.Gamma
import br.unb.cic.tp1.mh.visitors.Visitor

import scala.collection.mutable

class ExpLambda(val id : String, val tipoArgumento: Tipo, val corpo: Expressao) extends Expressao {

  override def avaliar(): Valor = {
     return Closure(id, tipoArgumento, corpo, Ambiente.ambienteAtual().clone())
  }

  override def verificaTipo: Tipo = {
    val t1 = tipoArgumento

    Gamma.mapear(id, t1)

    val t2 = corpo.verificaTipo

    return TArr(t1, t2)
  }

  override def aceitar(v: Visitor): Unit = {
    v.visitar(this)
  }
  
}
