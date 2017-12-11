package br.unb.cic.tp1.mh.ast

import br.unb.cic.tp1.mh.visitors._

trait Tipo {
  def nome : String
}

case class TErro() extends Tipo {
  override def nome = "TErro"
}
case class TInt() extends Tipo {
  override def nome = "Int"
}
case class TBool() extends Tipo {
  override def nome = "Bool"
}
case class TArr(val t1: Tipo, val t2: Tipo) extends Tipo {
  override def nome = t1.nome + " -> " + t2.nome
}


trait Expressao {
  def avaliar() : Valor
  def verificaTipo : Tipo
  def aceitar(v: Visitor) : Unit
}

