package br.unb.cic.tp1.mh.ast
import scala.collection.mutable.ListBuffer


class DecFuncao(val nome: String, val argsFormal : ListBuffer[(String,Tipo)], val corpo : Expressao)
