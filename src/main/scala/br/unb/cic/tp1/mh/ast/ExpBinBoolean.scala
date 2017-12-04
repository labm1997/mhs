package br.unb.cic.tp1.mh.ast
import br.unb.cic.tp1.mh.visitors.Visitor

object ExpressoesBinarias {
  def and(v1: ValorBooleano, v2: ValorBooleano): Boolean = v1.valor && v2.valor
  def or(v1: ValorBooleano, v2: ValorBooleano): Boolean = v1.valor || v2.valor
}

class ExpBool(val lhsC : Expressao, val rhsC : Expressao, injecao: (ValorBooleano,ValorBooleano) => Boolean) extends Expressao {
  var lhs = lhsC
  var rhs = rhsC
  override def avaliar(): Valor = {
    
    val v1 = lhs.avaliar().asInstanceOf[ValorBooleano]
    val v2 = rhs.avaliar().asInstanceOf[ValorBooleano]

    return ValorBooleano(injecao(v1,v2))
  }

  override def verificaTipo: Tipo = {
    val t1 = lhs.verificaTipo
    val t2 = rhs.verificaTipo

    if(t1 == TBool() && t2 == TBool()) {
      return TBool()
    }
    return TErro()
  }

  override def aceitar(v: Visitor): Unit = {
    v.visitar(this)
  }
}

case class ExpAnd(lhs: Expressao,rhs: Expressao) extends ExpBool(lhs,rhs,ExpressoesBinarias.and)
case class ExpOr(lhs: Expressao,rhs: Expressao) extends ExpBool(lhs,rhs,ExpressoesBinarias.or)

