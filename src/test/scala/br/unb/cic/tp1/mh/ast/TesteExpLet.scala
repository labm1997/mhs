package br.unb.cic.tp1.mh.ast

import org.scalatest._
import br.unb.cic.tp1.exceptions.VariavelNaoDeclaradaException
import br.unb.cic.tp1.mh.memoria.Ambiente

class TesteExpLet extends FlatSpec with Matchers {

  behavior of "a let expression"

  it should "return body type if tying is correct" in {
    Ambiente.iniciar()
    // let x = true in x + x
    val let = new ExpLet("x", ValorInteiro(10),
      new ExpSoma(new ExpRef("x"), new ExpRef("x")))

    let.verificaTipo should be (TInt())
  }

  it should "return error of typing" in {
    Ambiente.iniciar()
    // let x = true in x + x
    val let = new ExpLet("x", ValorBooleano(true),
      new ExpSoma(new ExpRef("x"), new ExpRef("x")))

    let.verificaTipo should  be (TErro())
  }
  
  it should "be evaluated to Valor(20) when let x = 10 in x + x" in {
    Ambiente.iniciar()
    val let  = new ExpLet("x", ValorInteiro(10),
      new ExpSoma(new ExpRef("x"), new ExpRef("x")))


    let.avaliar() should be (ValorInteiro(20))
  }

  it should "be evaluated to Valor(30) when let x = 10 in let y = 20 in x + y" in {
    Ambiente.iniciar()
    val let1 = new ExpLet("y", ValorInteiro(20),
      new ExpSoma(new ExpRef("x"), new ExpRef("y")))

    val let2 = new ExpLet("x", ValorInteiro(10), let1)

    let2.avaliar() should be (ValorInteiro(30))
  }
  
  it should "throw VariavelNaoDeclarada when let x = 10 in x + y" in {
    Ambiente.iniciar()
    val let = new ExpLet("x", ValorInteiro(10),
      new ExpSoma(new ExpRef("x"), new ExpRef("y")))

    intercept[VariavelNaoDeclaradaException] {
       let.avaliar()
    }
  }
}
