package br.unb.cic.tp1.mh.ast

import br.unb.cic.tp1.mh.visitors.Visitor
import br.unb.cic.tp1.exceptions.notbooleanException

import scala.collection.mutable

class ExpNot(val expNot : Expressao) extends Expressao {

  override def avaliar(): Valor = {
    expNot.avaliar match{
      case ValorBooleano(v) => return ValorBooleano(!v)
      case _ => throw notbooleanException()
    }
  }

  override def verificaTipo: Tipo = {
    val t1 = expNot.verificaTipo
    
    if(t1.equals(TBool())){
      return TBool()
    }
    else{
      return TErro()
    }
  }

  override def aceitar(v: Visitor): Unit = {
    v.visitar(this)
  }
  
}  
