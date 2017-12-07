package br.unb.cic.tp1.mh.visitors
import br.unb.cic.tp1.mh.ast._

class PPVisitor extends Visitor {

  val sb = new StringBuilder("")

  override def visitar(exp: ValorInteiro): Unit = {
    sb.append(exp.valor.toString)
  }

  override def visitar(exp: ValorBooleano): Unit = {
    sb.append(exp.valor.toString)
  }

  override def visitar(exp: ExpSoma): Unit = {
    sb += '('
    exp.lhs.aceitar(this)
    sb += '+'
    exp.rhs.aceitar(this)
    sb += ')'
  }

  override def visitar(exp: ExpSub): Unit = {
    sb += '('
    exp.lhs.aceitar(this)
    sb += '-'
    exp.rhs.aceitar(this)
    sb += ')'
  }

  override def visitar(exp: ExpMult): Unit = {
    sb += '('
    exp.lhs.aceitar(this)
    sb += '*'
    exp.rhs.aceitar(this)
    sb += ')'
  }

  override def visitar(exp: ExpDiv): Unit = {
    sb += '('
    exp.lhs.aceitar(this)
    sb += '/'
    exp.rhs.aceitar(this)
    sb += ')'
  }

  override def visitar(exp: ExpAnd): Unit = {
    sb += '('
    exp.lhs.aceitar(this)
    sb += '&'
    exp.rhs.aceitar(this)
    sb += ')'
  }

  override def visitar(exp: ExpOr): Unit = {
    sb += '('
    exp.lhs.aceitar(this)
    sb += '|'
    exp.rhs.aceitar(this)
    sb += ')'
  }

  override def visitar(exp: ExpMaiorQue): Unit = {
    sb += '('
    exp.lhs.aceitar(this)
    sb += '>'
    exp.rhs.aceitar(this)
    sb += ')'
  }

  override def visitar(exp: ExpMenorQue): Unit = {
    sb += '('
    exp.lhs.aceitar(this)
    sb += '<'
    exp.rhs.aceitar(this)
    sb += ')'
  }

  override def visitar(exp: ExpIgual): Unit = {
    sb += '('
    exp.lhs.aceitar(this)
    sb += '='
    exp.rhs.aceitar(this)
    sb += ')'
  }

  override def visitar(exp: ExpLet): Unit = { 
    sb ++= "let " + exp.id + "="
    exp.expNomeada.aceitar(this)
    sb ++= " in {"
    exp.corpo.aceitar(this)  
    sb ++= "}"
  }

  override def visitar(exp: ExpLambda): Unit = { 
    sb ++= "L " + exp.id + ":" + exp.tipoArgumento + " {"
    exp.corpo.aceitar(this)
    sb ++= "}"
  }

  override def visitar(exp: ExpAplicacaoLambda): Unit = { 
    sb ++= "App {"
    exp.exp2.aceitar(this)
    sb ++= "} => {"
    exp.exp1.aceitar(this)
    sb ++= "}"
  }

  override def visitar(exp: ExpAplicacaoNomeada): Unit = { }

  override def visitar(exp: ExpRef): Unit = { 
    sb ++= "ExpRef(" + exp.variavel + ")"
  }

  override def visitar(exp: Closure): Unit = { }
    
}
