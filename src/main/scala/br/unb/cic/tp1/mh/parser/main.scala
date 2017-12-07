package br.unb.cic.tp1.mh.parser

import br.unb.cic.tp1.mh.ast._
import br.unb.cic.tp1.mh.visitors._
import scala.io.StdIn.readLine
import scala.util.parsing.combinator.syntactical._
import scala.util.parsing.combinator._


object MyParsers extends RegexParsers {
  val ident: Parser[String] = """[a-zA-Z_]\w*""".r
}

class ExprParser extends StandardTokenParsers {   
  lexical.delimiters ++= List("+","-","*","/","(",")","=",":","{","}")
  lexical.reserved ++= List("let","in","L","Int","Bool","App")
  
  def value = numericLit ^^ {s => ValorInteiro(s.toInt)} | ident ^^ {s => new ExpRef(s)}
  def factor: Parser[Expressao] = (value | "(" ~ expr ~ ")" ^^ { case "(" ~ x ~ ")" => x})
  def term = (mult | div | value)
  
  def sum = term ~ "+" ~ term ^^ { case left ~ "+" ~ right => new ExpSoma(left,right)}
  def sub = term ~ "-" ~ term ^^ { case left ~ "-" ~ right => new ExpSub(left,right)}
  def mult = factor ~ "*" ~ factor ^^ { case left ~ "*" ~ right => new ExpMult(left,right)}
  def div = factor ~ "/" ~ factor ^^ { case left ~ "/" ~ right => new ExpDiv(left,right)}
  
  def corpo = "{" ~ expr ~ "}" ^^ {case "{" ~ expr ~ "}" => expr} | expr
    
  def let: Parser[Expressao] = "let" ~ ident ~ "=" ~ expr ~ "in" ~ corpo ^^ {case "let" ~ id ~ "=" ~ expNomeada ~ "in" ~ corpo => new ExpLet(id,expNomeada,corpo)}
  
  def lambda: Parser[Expressao] = "L" ~ ident ~ ":" ~ tipo ~ expr ^^ {case "L" ~ id ~ ":" ~ tipo ~ corpo => new ExpLambda(id,tipo,corpo)}
  
  def lambdaAPP: Parser[Expressao] = "App" ~ lambda ~ expr ^^ {case "App" ~ lambda ~ expr => new ExpAplicacaoLambda(lambda,expr)}
  
  def tipo: Parser[Tipo] = "Int" ^^ ( x => TInt()) | "Bool" ^^ ( x => TBool())
  
  def expr = (sum | sub | mult | div | value | let | lambda | lambdaAPP)
  
  def parse(s: String) = {
    val tokens = new lexical.Scanner(s)
    phrase(expr)(tokens)
  }
  
  def apply(s: String): Expressao = {
    parse(s) match {
      case Success(tree,_) => tree
      case e: NoSuccess => 
        throw new IllegalArgumentException("Bad syntax: "+s)
    }
  }
}

object Main extends ExprParser {
  
  import sext._
    
  def main(args: Array[String]){
    var continuar: Boolean = true
    while(continuar){
      val pp = new PPVisitor()
      print("mhs> ")
      val line = readLine()
      println("Lido: " + line)
      
      if(line == "sair") continuar = false
      
      else {
        parse(line) match {
          case Success(tree,_) => {
            tree.aceitar(pp)
            println(pp.sb)
            println(tree.avaliar)
          }
          case e: NoSuccess => println("Erro de sintaxe: " + e)
        }
      }
      
    } 
  }
  
}
