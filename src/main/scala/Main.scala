package dev.university

import dev.university.WorkflowLexer.keywordsRegex

import scala.util.matching.Regex
import scala.util.parsing.combinator.RegexParsers
import scala.util.parsing.input.Positional

case class Location(line: Int, column: Int) {
  override def toString = s"$line:$column"
}

sealed trait WorkflowToken extends Positional {
//  override def toString: String = super.toString + s"${pos.line}:${pos.column}"
}

case class UNKOWNELEMENT(str: String) extends WorkflowToken
case class CONSTANT(str: String) extends WorkflowToken
case class SEPARATOR(str: String) extends WorkflowToken
case class OPERATOR(str: String) extends WorkflowToken
case class KEYWORD(str: String) extends WorkflowToken
case class IDENTIFIER(str: String) extends WorkflowToken
case class LITERAL(str: String) extends WorkflowToken
case class COMMENT(str: String) extends WorkflowToken
case class NEWLINE(str: String) extends WorkflowToken


trait WorkflowCompilationError
case class WorkflowLexerError(msg: String) extends WorkflowCompilationError

object WorkflowLexer extends RegexParsers {
  override def skipWhitespace = true
  override val whiteSpace: Regex = "[ \t\r\f]+".r

  def unkownElement: Parser[UNKOWNELEMENT] = positioned {
    "^(?!\\s*$).+".r ^^ {str => UNKOWNELEMENT(str)}
  }

  private val keywordsList = List("auto", "break", "case", "char", "const", "continue",
    "default", "do", "double", "else", "enum", "extern", "float",
    "for",	"goto",	"if", "int", "long", "register", "return",
    "short", "signed", "sizeof", "static",
    "struct", "switch", "typedef", "union",
    "unsigned",	"void",	"volatile",	"while", "struct", "class", "private", "public", "protected")
  val keywordsRegex: String = keywordsList.foldRight("auto") { (acc, it) =>
    acc + "|" + it
  }

  private val operatorsList = List("=", "+","-","/","*",">","<",">>","<<", "|", "||", "&", "&&", "%", "!")
  private val operatorsRegex = "^(" + operatorsList.map{it => "\\" + it} .foldRight("auto") { (acc, it) =>
    acc + "|" + it
  } + ")"

  def newLine: Parser[NEWLINE] = positioned {
    "\\n".r ^^ {str => NEWLINE("")}
  }

  //"/\\*[^*]*\\*+(?:[^/*][^*]*\\*+)*/"
  def singleLineComment: Parser[COMMENT] = positioned {
    "^(//)[^\\n\\r]*[\\n\\r].".r ^^ { str => COMMENT(str)}
  }

  def multiLineCOmment: Parser[COMMENT] = positioned {
    "/\\*[^*]*\\*+(?:[^/*][^*]*\\*+)*/".r ^^ {str => COMMENT(str)}
  }

  def constant: Parser[CONSTANT] = positioned {
    "\\d+".r ^^ {str => CONSTANT(str)}
  }

  def separator: Parser[SEPARATOR] = positioned {
    "[(){},;:]".r ^^ { str => SEPARATOR(str)}
  }

  def operator: Parser[OPERATOR] = positioned {
    "^(\\+|-|\\*|\\/|=|\\<<|>|<|>=|<=|&|\\||%|!|\\^)".r ^^ {str => OPERATOR(str)}
  }

  def keyword: Parser[KEYWORD] = positioned {
    s"\\b(?:$keywordsRegex)\\b".r ^^ { str => KEYWORD(str) }
  }

  def identifier: Parser[IDENTIFIER] = positioned {
    "[a-zA-Z_][a-zA-Z0-9_]*".r ^^ { str => IDENTIFIER(str) }
  }

  def literal: Parser[LITERAL] = positioned {
    """"[^"]*"""".r ^^ { str =>
      val content = str.substring(1, str.length - 1)
      LITERAL(content)
    }
  }




  def tokens: Parser[List[WorkflowToken]] = {
    phrase(rep1(newLine | singleLineComment | multiLineCOmment | operator | keyword | literal | identifier | separator | constant | unkownElement))
  }

  def apply(code: String): Either[WorkflowLexerError, List[WorkflowToken]] = {
    parse(tokens, code) match {
      case NoSuccess(msg, next) => Left(WorkflowLexerError(msg))
      case Success(result, next) => Right(result)
    }
  }
}

object Main {
  def main(args: Array[String]): Unit = {

    val sourceCode = scala.io.Source.fromFile("test.cpp").mkString.concat("\n").dropRight(1)
    println(sourceCode)
    val tokensResult = WorkflowLexer(sourceCode)
    if (tokensResult.isRight) {
      print(tokensResult.right.filter{it =>
        it != NEWLINE
      })
    }
//    print(WorkflowLexer(sourceCode))
  }
}
