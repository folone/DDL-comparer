package info.folone.ddl

import scala.util.parsing.combinator._
import scala.util.parsing.combinator.syntactical._

object DDLParser extends JavaTokenParsers {
  val tableName  = """[a-zA-Z]+""".r
  val columnName = tableName
  val indexName  = tableName
  val dataType   = "INT" | "SHIT"
  val statementTermination = ";"
  val columnDelimiter = ",*".r

  case class CreateTable(name: String, columns: Map[String, String])

  // Handle comments
  protected override val whiteSpace = """(\s|#.*|(?m)/\*(\*(?!/)|[^*])*\*/;*)+""".r

  def createTable = "CREATE" ~ "TABLE" ~ tableName ~
    "(" ~ ((columnName ~ dataType ~ columnDelimiter)*) ~ ")" ^^ {
      case _ ~ _ ~ name ~ _ ~ columns ~ _ =>
        CreateTable(name, columns.map(x => (x._1._1,x._1._2)).toMap)
    }
  def dropTable = "DROP" ~ "TABLE" ~ tableName
  def statement = (createTable | dropTable) ~ statementTermination ^^ { case res ~ _ => res }
  def program   = statement*

  def parse(sql: String) = parseAll(program, sql) map (_.toSet)
}
