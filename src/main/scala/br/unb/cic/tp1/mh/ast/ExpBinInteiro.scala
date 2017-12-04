package br.unb.cic.tp1.mh.ast
import br.unb.cic.tp1.mh.visitors.Visitor

object ExpressoesBinariasInt{
  def mult(v1: ValorInteiro, v2: ValorInteiro): Int = v1.valor * v2.valor
  def div(v1: ValorInteiro, v2: ValorInteiro): Int = v1.valor / v2.valor
  def soma(v1: ValorInteiro, v2: ValorInteiro): Int = v1.valor + v2.valor
  def sub(v1: ValorInteiro, v2: ValorInteiro): Int = v1.valor - v2.valor
}

class ExpInt(lhsC : Expressao, rhsC : Expressao, injecao: (ValorInteiro,ValorInteiro) => Int) extends Expressao {
  var lhs = lhsC
  var rhs = rhsC
  override def avaliar(): Valor = {
    
    val v1 = lhs.avaliar().asInstanceOf[ValorInteiro]
    val v2 = rhs.avaliar().asInstanceOf[ValorInteiro]

    return ValorInteiro(injecao(v1,v2))
  }

  override def verificaTipo: Tipo = {
    val t1 = lhs.verificaTipo
    val t2 = rhs.verificaTipo

    if(t1 == TInt() && t2 == TInt()) {
      return TInt()
    }
    return TInt()
  }

  override def aceitar(v: Visitor): Unit = {
    v.visitar(this)
  }
}

case class ExpSoma(lhs: Expressao,rhs: Expressao) extends ExpInt(lhs,rhs,ExpressoesBinariasInt.soma)
case class ExpMult(lhs: Expressao,rhs: Expressao) extends ExpInt(lhs,rhs,ExpressoesBinariasInt.mult)
case class ExpDiv(lhs: Expressao,rhs: Expressao) extends ExpInt(lhs,rhs,ExpressoesBinariasInt.div)
case class ExpSub(lhs: Expressao,rhs: Expressao) extends ExpInt(lhs,rhs,ExpressoesBinariasInt.sub)

