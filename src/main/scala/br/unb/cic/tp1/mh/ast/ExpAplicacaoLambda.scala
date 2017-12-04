package br.unb.cic.tp1.mh.ast

import br.unb.cic.tp1.mh.memoria.Ambiente
import br.unb.cic.tp1.exceptions.ExpressaoInvalida
import br.unb.cic.tp1.mh.visitors.Visitor

case class ExpAplicacaoLambda(exp1 : Expressao, exp2 : Expressao) extends Expressao { 

  override def avaliar(): Valor = {
    val v1 = exp1.avaliar()
    v1 match {
      case Closure(v, tipo, c, ambiente) => {
        Ambiente.novoAmbiente(ambiente)
        Ambiente.atualiza(v, exp2.avaliar())
	      val res =  c.avaliar()
        Ambiente.removeAmbiente()
        return res
      }
      case _             => throw ExpressaoInvalida()
    } 
  }

  override def verificaTipo: Tipo = {
    val t1 = exp1.verificaTipo
    val t2 = exp2.verificaTipo

    t1 match {
      case TArr(tArg, tRet) if (tArg.equals(t2)) => tRet
      case _ => TErro()
    }
  }

  override def aceitar(v: Visitor): Unit = {
    v.visitar(this)
  }
  
}
