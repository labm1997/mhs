package br.unb.cic.tp1.mh.parser

import br.unb.cic.tp1.mh.ast._
import br.unb.cic.tp1.exceptions._
import br.unb.cic.tp1.mh.visitors._
import scala.io.StdIn.readLine
import scala.util.parsing.combinator.syntactical._
import scala.util.parsing.combinator._
import scala.collection.mutable.ListBuffer
import br.unb.cic.tp1.mh.memoria.Ambiente


object MyParsers extends RegexParsers {
  val ident: Parser[String] = """[a-zA-Z_]\w*""".r
} 
class ExprParser extends StandardTokenParsers {   
  lexical.delimiters ++= List("+","-","*","/","(",")","=",":","{","}","==","&&","||",">","<",",")
  lexical.reserved ++= List("let","in","L","Int","Bool","App","true","false","def")
  
  def value = numericLit ^^ {s => ValorInteiro(s.toInt)} | ident ^^ {s => new ExpRef(s)}
  def factor: Parser[Expressao] = (value | "(" ~ expr ~ ")" ^^ { 
    case "(" ~ x ~ ")" => x 
    case _ => throw ExpressaoInvalida() 
  })
  
  def term = (multdiv | value)
  
  def boolean: Parser[Expressao] = igual | maiorque | menorque | "true" ^^ {x => new ValorBooleano(true)} | "false" ^^ {x => new ValorBooleano(false)}  | ident ^^ {s => new ExpRef(s)} | sumsub
    
  def sumsub = term ~ rep("+" ~ term | "-" ~ term) ^^ { 
    case left ~ right => {
      var rep = left
      right.foreach(rel => rel match { 
        case "+" ~ r => rep = new ExpSoma(rep,r)
        case "-" ~ r => rep = new ExpSub(rep,r)
        case _ => throw ExpressaoInvalida()
      })
      rep
    }
  }
  def multdiv: Parser[Expressao] = factor ~ rep("*" ~ factor | "/" ~ factor) ^^ { 
    case left ~ right => {
      var rep = left
      right.foreach(rel => rel match { 
        case "*" ~ r => rep = new ExpMult(rep,r)
        case "/" ~ r => rep = new ExpDiv(rep,r)
        case _ => throw ExpressaoInvalida()
      })
      rep
    }
  }
  
  def igual: Parser[Expressao] = (term ~ "==" ~ term) ^^ {case left ~ "==" ~ right => new ExpIgual(left,right)}
  
  def maiorque: Parser[Expressao] = (term ~ ">" ~ term) ^^ {case left ~ ">" ~ right => new ExpMaiorQue(left,right)}
  
  def menorque: Parser[Expressao] = (term ~ "<" ~ term) ^^ {case left ~ "<" ~ right => new ExpMenorQue(left,right)}
  
  def andor: Parser[Expressao] = boolean ~ rep("&&" ~ boolean | "||" ~ boolean) ^^ { 
    case left ~ right => {
      var rep = left
      right.foreach(rel => rel match { 
        case "&&" ~ r => rep = new ExpAnd(rep,r)
        case "||" ~ r => rep = new ExpOr(rep,r)
        case _ => throw ExpressaoInvalida()
      })
      rep
    }
  }
      
  def corpo = "{" ~ expr ~ "}" ^^ {case "{" ~ expr ~ "}" => expr case _ => throw ExpressaoInvalida()} | expr
    
  def let: Parser[Expressao] = "let" ~ ident ~ "=" ~ expr ~ "in" ~ corpo ^^ {case "let" ~ id ~ "=" ~ expNomeada ~ "in" ~ corpo => new ExpLet(id,expNomeada,corpo)}
  
  def lambda: Parser[Expressao] = "L" ~ ident ~ ":" ~ tipo ~ expr ^^ {case "L" ~ id ~ ":" ~ tipo ~ corpo => new ExpLambda(id,tipo,corpo)}
  
  def defFuncao: Parser[DecFuncao] = "def" ~ ident ~ "(" ~ rep(ident ~ ":" ~ tipo ~ opt(",")) ~ ")" ~ expr ^^ {
    case "def" ~ nome ~ "(" ~ args ~ ")" ~ corpo => {
      val func = new DecFuncao(nome, args.map(arg => arg match {case id ~ ":" ~ tipo ~ rest => (id,tipo)}).to[ListBuffer], corpo)
      Ambiente.declararFuncao(func)
      func
    }
  }
  
  def defFuncaoAPP: Parser[Expressao] = ident ~ "(" ~ rep(expr ~ opt(",")) ~ ")" ^^ {
    case id ~ "(" ~ args ~ ")" => 
    new ExpAplicacaoNomeada(id,args.map(arg => arg match {case a ~ r => a}).to[ListBuffer])
  }
  
  def lambdaAPP: Parser[Expressao] = "App" ~ lambda ~ expr ^^ {case "App" ~ lambda ~ expr => new ExpAplicacaoLambda(lambda,expr)}
  
  def tipo: Parser[Tipo] = "Int" ^^ ( x => TInt()) | "Bool" ^^ ( x => TBool())
  
  def expr = (andor | igual | maiorque | menorque | defFuncaoAPP | sumsub | multdiv | let | lambda | lambdaAPP | value | boolean)
  
  def evaluate = defFuncao | expr
  
  def parse(s: String) = {
    val tokens = new lexical.Scanner(s)
    phrase(evaluate)(tokens)
  }
  
  def apply(s: String): AnyRef = {
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
      
      if(line == "sair") continuar = false
      
      else if(line != "") {
        parse(line) match {
          case Success(tree,_) => {
            tree match {
              case t: Expressao => {
                t.aceitar(pp)
                println(pp.sb)
                println(t.avaliar)
              }
              case t: DecFuncao => {
                println("Definido")
              }
            }
          }
          case e: NoSuccess => println("Erro de sintaxe: " + e)
        }
      }
      
    } 
  }
  
}
