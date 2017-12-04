package br.unb.cic.tp1.mh.memoria

import br.unb.cic.tp1.mh.ast._
import scala.collection.mutable

object Gama {

  private val symbolTable = new mutable.HashMap[String, Tipo]

  def encontrar( simbolo: String ): Tipo = {
    symbolTable(simbolo)
  }

  def salvar( simbolo: String, tipo: Tipo ): Unit = {
    symbolTable += (simbolo -> tipo)
  }

}