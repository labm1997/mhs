package br.unb.cic.tp1.mh.ast

import br.unb.cic.tp1.mh.visitors.Visitor
import scala.reflect.ClassTag
import scala.reflect._

class ExpSoma(lhs: Expressao, rhs: Expressao) extends ExpBinaria[ValorInteiro,Int,TInt,java.lang.Integer](lhs,rhs,ExpOperacoes.soma) {
  override def aceitar(v: Visitor): Unit = {
    v.visitar(this)
  }
}

class ExpSub(lhs: Expressao, rhs: Expressao) extends ExpBinaria[ValorInteiro,Int,TInt,java.lang.Integer](lhs,rhs,ExpOperacoes.subtracao) {
  override def aceitar(v: Visitor): Unit = {
    v.visitar(this)
  }
}

class ExpMult(lhs: Expressao, rhs: Expressao) extends ExpBinaria[ValorInteiro,Int,TInt,java.lang.Integer](lhs,rhs,ExpOperacoes.multiplicacao) {
  override def aceitar(v: Visitor): Unit = {
    v.visitar(this)
  }
}

class ExpDiv(lhs: Expressao, rhs: Expressao) extends ExpBinaria[ValorInteiro,Int,TInt,java.lang.Integer](lhs,rhs,ExpOperacoes.divisao) {
  override def aceitar(v: Visitor): Unit = {
    v.visitar(this)
  }
}

class ExpAnd(lhs: Expressao, rhs: Expressao) extends ExpBinaria[ValorBooleano,Boolean,TBool,java.lang.Boolean](lhs,rhs,ExpOperacoes.and) {
  override def aceitar(v: Visitor): Unit = {
    v.visitar(this)
  }
}

class ExpOr(lhs: Expressao, rhs: Expressao) extends ExpBinaria[ValorBooleano,Boolean,TBool,java.lang.Boolean](lhs,rhs,ExpOperacoes.or) {
  override def aceitar(v: Visitor): Unit = {
    v.visitar(this)
  }
}

class ExpMaiorQue(lhs: Expressao, rhs: Expressao) 
  extends ExpBinaria[ValorInteiro, Boolean, TInt, java.lang.Boolean](lhs,rhs,ExpOperacoes.maiorQue) {
  override def aceitar(v: Visitor): Unit = {
    v.visitar(this)
  }
}

class ExpMenorQue(lhs: Expressao, rhs: Expressao) 
  extends ExpBinaria[ValorInteiro, Boolean, TInt, java.lang.Boolean](lhs,rhs,ExpOperacoes.menorQue) {
  override def aceitar(v: Visitor): Unit = {
    v.visitar(this)
  }
}

class ExpIgual(lhs: Expressao, rhs: Expressao) 
  extends ExpBinaria[ValorInteiro, Boolean, TInt, java.lang.Boolean](lhs,rhs,ExpOperacoes.igual) {
  override def aceitar(v: Visitor): Unit = {
    v.visitar(this)
  }
}


trait ExpOperacao[V,T] {
  def operacao(v1: V, v2: V): T
}

object ExpOperacoes {
  val soma = new ExpOperacao[ValorInteiro, Int] {
    def operacao(v1: ValorInteiro, v2: ValorInteiro): Int = v1.valor + v2.valor
  }
  val subtracao = new ExpOperacao[ValorInteiro, Int] {
    def operacao(v1: ValorInteiro, v2: ValorInteiro): Int = v1.valor - v2.valor
  }
  val divisao = new ExpOperacao[ValorInteiro, Int] {
    def operacao(v1: ValorInteiro, v2: ValorInteiro): Int = v1.valor / v2.valor
  }
  val multiplicacao = new ExpOperacao[ValorInteiro, Int] {
    def operacao(v1: ValorInteiro, v2: ValorInteiro): Int = v1.valor * v2.valor
  }
  val and = new ExpOperacao[ValorBooleano, Boolean] {
    def operacao(v1: ValorBooleano, v2: ValorBooleano): Boolean = v1.valor && v2.valor
  }
  val or = new ExpOperacao[ValorBooleano, Boolean] {
    def operacao(v1: ValorBooleano, v2: ValorBooleano): Boolean = v1.valor || v2.valor
  }
  val maiorQue = new ExpOperacao[ValorInteiro, Boolean] {
    def operacao(v1: ValorInteiro, v2: ValorInteiro): Boolean = v1.valor > v2.valor
  }
  val menorQue = new ExpOperacao[ValorInteiro, Boolean] {
    def operacao(v1: ValorInteiro, v2: ValorInteiro): Boolean = v1.valor < v2.valor
  }
  val igual = new ExpOperacao[ValorInteiro, Boolean] {
    def operacao(v1: ValorInteiro, v2: ValorInteiro): Boolean = v1.valor == v2.valor
  }
}

object Instanciador {
  def instanciar(tipo: Class[_], argumento: AnyRef): AnyRef = {
    var arg = Array[AnyRef](argumento)
    return tipo.getConstructors.head.newInstance(arg:_*).asInstanceOf[AnyRef]
  }
}

abstract class ExpBinaria[V <: Valor : ClassTag,T,G <: Tipo : ClassTag, TipoConstrutor : ClassTag](lhsConstructor : Expressao, rhsConstructor : Expressao, injecao: ExpOperacao[V,T]) extends Expressao {
  
  var lhs = lhsConstructor
  var rhs = rhsConstructor
  
  override def avaliar(): Valor = {
    val v1 = lhs.avaliar().asInstanceOf[V]
    val v2 = rhs.avaliar().asInstanceOf[V]
    
    /* Instanciamos o argumento a ser usado no construtor de ValorInteiro ou ValorBooleano */
    val arg = Instanciador.instanciar(classTag[TipoConstrutor].runtimeClass, injecao.operacao(v1,v2).asInstanceOf[AnyRef])
    
    /* Instanciamos ValorInteiro ou ValorBooleano a partir do argumento gerado */
    return Instanciador.instanciar(classTag[V].runtimeClass, arg).asInstanceOf[V]
    
  }

  override def verificaTipo: Tipo = {
    val t1 = lhs.verificaTipo
    val t2 = rhs.verificaTipo

    if(t1.getClass == classTag[G].runtimeClass && t2.getClass == classTag[G].runtimeClass) {
      return classTag[G].runtimeClass.newInstance.asInstanceOf[G]
    }
    return TErro()
  }  
}

