package br.unb.cic.tp1.mh.ast
import br.unb.cic.tp1.mh.visitors.Visitor

case class ValorBooleano(v : Boolean) extends ValorConcreto[Boolean](v) {
  override def verificaTipo: Tipo = TBool()

  override def aceitar(v: Visitor): Unit = {
    v.visitar(this)
  }
}
