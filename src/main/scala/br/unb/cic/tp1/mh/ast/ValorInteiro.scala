package br.unb.cic.tp1.mh.ast
import br.unb.cic.tp1.mh.visitors.Visitor

case class ValorInteiro(v : Integer) extends ValorConcreto[Integer](v) {
  override def verificaTipo: Tipo = TInt()

  override def aceitar(v: Visitor): Unit = {
    v.visitar(this)
  }
}
