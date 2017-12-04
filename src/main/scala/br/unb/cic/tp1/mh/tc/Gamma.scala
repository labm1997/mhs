package br.unb.cic.tp1.mh.tc

import br.unb.cic.tp1.mh.ast.Tipo

import scala.collection.mutable

object Gamma {

  private val mapeamento = new mutable.HashMap[String, Tipo]()

  def mapear(variavel: String, tipo: Tipo): Unit = {
    mapeamento  += (variavel -> tipo)
  }

  def consultar(variavel: String) = mapeamento(variavel)

}
