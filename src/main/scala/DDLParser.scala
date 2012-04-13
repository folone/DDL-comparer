package info.folone.ddl

import scala.util.parsing.combinator._
import scala.util.parsing.combinator.syntactical._

object DDLParser extends JavaTokenParsers {
  val tableName  = """(`)?[a-zA-Z]+(`)?""".r
  val columnName = tableName
  val indexName  = tableName
  val default    = tableName
  val dataType   = """[a-zA-Z]+(\([0-9]+\))?""".r
  val statementTermination = ";"
  val columnDelimiter = """,*""".r

  // Columns: column name, data type, not null, autoincr, default value
  case class CreateTable(name: String, columns: List[(String, String, Boolean, Boolean, Option[String])])

  def cleanString(str: String) = str.replaceAll("`", "")

  // Handle comments
  protected override val whiteSpace = """(\s|#.*|(?m)/\*(\*(?!/)|[^*])*\*/;*)+""".r

  def createTable = ("""(?i)CREATE TABLE""".r) ~ ("""(?i)IF NOT EXISTS""".r?) ~ tableName ~
    "(" ~
      ((columnName ~ dataType ~
        ("""(?i)NOT NULL""".r?) ~ ("""(?i)AUTO_INCREMENT""".r?) ~ ((("""(?i)DEFAULT""".r) ~ default)?) ~
        columnDelimiter)*) ~
      /*((("""(?i)PRIMARY KEY""".r) ~ "(" ~ columnName ~ ")" ~ columnDelimiter)?) ~
      ((("""(?i)UNIQUE""".r?) ~ ("""(?i)KEY""".r) ~ indexName ~ "(" ~ columnName ~ ")" ~ columnDelimiter)?) ~
      */
    ")" ^^ {
      case _ ~ _ ~ name ~ "(" ~ columns ~ ")" => {
        val columnsData = columns map { entry =>
          entry match {
            case colName ~ colType ~ notNull ~ autoInc ~ isDefault ~ _ =>
              (cleanString(colName), colType, notNull.isDefined, autoInc.isDefined, isDefault.map(_._2))
          }
        }
        CreateTable(cleanString(name), columnsData)
      }
    }
  def dropTable = "(?i)DROP" ~ "(?i)TABLE" ~ tableName
  def statement = (createTable | dropTable) ~ statementTermination ^^ { case res ~ _ => res }
  def program   = statement*

  def parse(sql: String) = parseAll(program, sql) map (_.toSet)
}
