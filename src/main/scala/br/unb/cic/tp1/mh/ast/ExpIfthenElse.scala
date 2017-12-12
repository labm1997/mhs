package br.unb.cic.tp1.mh.ast

import br.unb.cic.tp1.mh.visitors.Visitor

import scala.collection.mutable

class ExpIfthenElse(val condicao : Expressao, val verdadeira: Expressao, val falsa: Expressao) extends Expressao {

  override def avaliar(): Valor = {
     if(condicao.avaliar == ValorBooleano(true)){
       return verdadeira.avaliar
     }
     else{ 
       return falsa.avaliar
     }
  }

  override def verificaTipo: Tipo = {
    val t1 = condicao.verificaTipo
    val t2 = verdadeira.verificaTipo
    val t3 = falsa.verificaTipo
    
    if(t1.equals(TBool()) && t2.equals(t3)){
      return t2
    }
    else{
      return TErro()
    }
  }

  override def aceitar(v: Visitor): Unit = {
    v.visitar(this)
  }
  
} 
