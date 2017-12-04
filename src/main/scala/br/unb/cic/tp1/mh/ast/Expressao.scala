package br.unb.cic.tp1.mh.ast

import br.unb.cic.tp1.mh.visitors.Visitor

trait Tipo

case class TErro() extends Tipo
case class TInt() extends Tipo
case class TBool() extends Tipo
case class TArr(val t1: Tipo, val t2: Tipo) extends Tipo


trait Expressao {
  def avaliar() : Valor
  def verificaTipo : Tipo
  def aceitar(v: Visitor) : Unit
}

