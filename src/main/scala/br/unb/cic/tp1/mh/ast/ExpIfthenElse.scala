package br.unb.cic.tp1.mh.ast

import br.unb.cic.tp1.mh.memoria.Ambiente
import br.unb.cic.tp1.mh.tc.Gamma
import br.unb.cic.tp1.mh.visitors.Visitor

import scala.collection.mutable

// Todas as outras expressoes comecam com string, mas acho que aqui posso tirar, pois os argumentos são só Expressao
class ExpIfthenElse(val condicao : Expressao, val verdadeira: Expressao, val falsa: Expressao) extends Expressao {

  override def avaliar(): Valor = {
     if(condicao == true){
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
    // Como cada Expressao tem seu verificaTipo resolvi chamar todos aqui, mas o TArr, recebe só dois argumentos, como passo o terceiro?
    return TArr(t1, t2)
  }

  override def aceitar(v: Visitor): Unit = {
    v.visitar(this)
  }
  
} 
