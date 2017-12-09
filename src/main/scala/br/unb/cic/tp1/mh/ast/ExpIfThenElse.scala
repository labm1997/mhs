 package br.unb.cic.tp1.mh.ast

import br.unb.cic.tp1.mh.visitors.Visitor
import scala.reflect.ClassTag
import scala.reflect._


 
class ExpIfthenElse(condicao: Expressao, verdadeira: Expressao, falsa: Expressao) 
  extends ExpBinariaif[Expressao,Expressao,TBool,java.lang.Boolean, Expressao](condicao, verdadeira, falsa, ExpOperacaoif_then_else.ifthenelse){
  override def aceitar(v: Visitor): Unit = {
    v.visitar(this)
  }
}

trait ExpOperacaoif[V,T] {
  def operacao(v1: V, v2: V, v3: V): T
}

object ExpOperacaoif_then_else{
  val ifthenelse = new ExpOperacaoif[Expressao,Expressao]{
    def operacao(v1: Expressao, v2: Expressao, v3: Expressao): Expressao = {
      if(v1 == true)
        return v2.avaliar()
      else
        return v3.avaliar()
    }
  }
}

object Instanciadorif {
  def instanciar(tipo: Class[_], argumento: AnyRef): AnyRef = {
    var arg = Array[AnyRef](argumento)
    return tipo.getConstructors.head.newInstance(arg:_*).asInstanceOf[AnyRef]
  }
}

abstract class ExpBinariaif[V <: Expressao : ClassTag,T,G <: Tipo : ClassTag, TipoConstrutor : ClassTag, Saida <: Expressao : ClassTag](condicao : Expressao, verdadeira : Expressao, falsa: Expressao, injecao: ExpOperacaoif[V,T]) extends Expressao {
  
  var con = condicao
  var vdd = verdadeira
  var fal = falsa
  
  override def avaliar(): Valor = {
    val v1 = con.avaliar().asInstanceOf[V]
    val v2 = vdd.avaliar().asInstanceOf[V]
    val v3 = fal.avaliar().asInstanceOf[V]
    
    val arg = Instanciadorif.instanciar(classTag[TipoConstrutor].runtimeClass, injecao.operacao(v1,v2,v3).asInstanceOf[AnyRef])
    
    return Instanciadorif.instanciar(classTag[Saida].runtimeClass, arg).asInstanceOf[Saida]
    
  }

  override def verificaTipo: Tipo = {
    val t1 = con.verificaTipo
    val t2 = vdd.verificaTipo
    val t3 = fal.verificaTipo

    if(t1.getClass == classTag[G].runtimeClass && t2.getClass == classTag[G].runtimeClass && t3.getClass == classTag[G]) {
      return classTag[G].runtimeClass.newInstance.asInstanceOf[G]
    }
    return TErro()
  }  
}
