package br.unb.cic.tp1.mh.ast

import br.unb.cic.tp1.mh.memoria.Ambiente
import br.unb.cic.tp1.mh.visitors.Visitor
import scala.collection.mutable.ListBuffer

class ExpAplicacaoNomeada(val nome: String, val argumentosAtual : ListBuffer[Expressao]) extends Expressao {

  override def avaliar(): Valor = {
    val decFuncao = Ambiente.recuperarFuncao(nome)

    Ambiente.novoAmbiente(Ambiente.ambienteAtual().clone())
    
    /* Avalia cada argumento passado e salva a referencia no ambiente */
    argumentosAtual.zipWithIndex.foreach(
      arg => Ambiente.atualiza(decFuncao.argsFormal(arg._2)._1, arg._1.avaliar())
    )

    val res = decFuncao.corpo.avaliar()

    Ambiente.removeAmbiente()

    return res
  }

  override def verificaTipo: Tipo = {
    val decFuncao = Ambiente.recuperarFuncao(nome)
    
    /* Número incorreto de argumentos passados para a expressão nomeada */
    if(argumentosAtual.length != decFuncao.argsFormal.length)
      return TErro()
    
    /* Argumentos com tipos incorretos */
    for(arg <- argumentosAtual.zipWithIndex){
      if(!decFuncao.argsFormal(arg._2)._2.equals(arg._1.verificaTipo)) return TErro()
    }
    
    /* Retorna o tipo do corpo */
    return decFuncao.corpo.verificaTipo
      
  }

  override def aceitar(v: Visitor): Unit = {
    v.visitar(this)
  }
}
