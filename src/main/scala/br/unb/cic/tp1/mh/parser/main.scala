package br.unb.cic.tp1.mh.parser

import br.unb.cic.tp1.mh.ast._
import br.unb.cic.tp1.mh.visitors._
import scala.io.StdIn.readLine
import scala.util.parsing.combinator.syntactical.StandardTokenParsers

object Parser {
  val operacoes = Array[Char]('+','-','/','*')
  def operacao(str: String): Option[Expressao] = {
    for(op <- operacoes) {
      val index = str.indexOf(op)
      if(index != -1) {
        val corte = str.splitAt(index)
        var exp: Expressao = null
        if(op == '+') exp = new ExpSoma(ValorInteiro(corte._1.toInt), ValorInteiro(corte._2.tail.toInt))
        else if(op == '-') exp = new ExpSub(ValorInteiro(corte._1.toInt), ValorInteiro(corte._2.tail.toInt))
        else if(op == '/') exp = new ExpDiv(ValorInteiro(corte._1.toInt), ValorInteiro(corte._2.tail.toInt))
        else if(op == '*') exp = new ExpMult(ValorInteiro(corte._1.toInt), ValorInteiro(corte._2.tail.toInt))
        
        return Some(exp.avaliar)
      }
    }
    return None
  }
}

object Main {
    
  def main(args: Array[String]){
    var continuar: Boolean = true
    while(continuar){
      val pp = new PPVisitor()
      print("mhs> ")
      val line = readLine()
      println("Lido: " + line)
      
      val result = Parser.operacao(line)
      result match {
      
        case Some(r) => {
          r.aceitar(pp)
          println(pp.sb.toString)
        }
        case None => println("Erro de sintaxe")
      }
      
      
      if(line == "sair") continuar = false
    } 
  }
  
}
