package br.unb.cic.tp1.mh.ast

import br.unb.cic.tp1.mh.memoria.Ambiente
import br.unb.cic.tp1.mh.visitors.Visitor

class ExpAplicacaoNomeada(val nome: String, val argumentoAtual : Expressao) extends Expressao {

  override def avaliar(): Valor = {
    val decFuncao = Ambiente.recuperarFuncao(nome)

    Ambiente.novoAmbiente()
    Ambiente.atualiza(decFuncao.argFormal, argumentoAtual.avaliar())

    val res = decFuncao.corpo.avaliar()

    Ambiente.removeAmbiente()

    return res
  }

  override def verificaTipo: Tipo = TErro()

  override def aceitar(v: Visitor): Unit = {
    v.visitar(this)
  }
}
