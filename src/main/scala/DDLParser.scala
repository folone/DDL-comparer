package info.folone.ddl

import scala.util.parsing.combinator._
import scala.util.parsing.combinator.syntactical._

object DDLParser extends JavaTokenParsers {
  val tableName  = """(?!(?i)KEY)(?!(?i)PRIMARY)(?!(?i)UNIQUE)(`)?[a-zA-Z_0-9]+(`)?""".r
  val columnName = tableName
  val indexName  = tableName
  val default    = tableName
  val keyName    = tableName
  val engine     = tableName
  val charset    = tableName
  val dataType   = """[a-zA-Z]+(\([0-9]+\))?""".r
  val statementTermination = ";"
  val columnDelimiter = """,*""".r

  final case class Table(name: String, columns: Set[Column], constraints: Set[Constraint])
  final case class Column(name: String, datatype: String, notNull: Boolean,
                    autoInc: Boolean, defaultVal: Option[String])
  sealed abstract case class Constraint
  final case class UniqueKey(name: Option[String], column: String) extends Constraint
  final case class PrimaryKey(column: String) extends Constraint
  final case class ForeignKey(name: String, column: String, foreignTable: String,
                              foreignColumn: String) extends Constraint
  final case class Key(name: Option[String], column: String) extends Constraint

  def cleanString(str: String) = str.replaceAll("`", "")

  // Handle comments
  protected override val whiteSpace = """(\s|#.*|(?m)/\*(\*(?!/)|[^*])*\*/;*)+""".r

  def column = columnName ~ dataType     ~
    ("""(?i)NOT NULL""".r?)              ~
    ("""(?i)AUTO_INCREMENT""".r?)        ~
    ((("""(?i)DEFAULT""".r) ~ default)?) ~
    columnDelimiter

  def uniqueOrPk = ("""(?i)(PRIMARY|UNIQUE)""".r?) ~ ("""(?i)KEY""".r) ~
    (keyName?) ~ "(" ~ columnName ~ ")" ~ columnDelimiter ^^ {
      case kind ~ _ ~ name ~ "(" ~ column ~ ")" ~ _ =>
        kind match {
          case Some(x) if x.equalsIgnoreCase("primary") => PrimaryKey(column)
          case Some(x) if x.equalsIgnoreCase("unique")  => UniqueKey(name, column)
          case None => Key(name, column)
        }
    }

  def fk = """(?i)CONSTRAINT""".r ~ keyName ~ """FOREIGN KEY""".r ~
    "(" ~ columnName ~ ")" ~ """(?i)REFERENCES""".r ~
    tableName ~ "(" ~ columnName ~ ")" ~ columnDelimiter ^^ {
      case _ ~ keyName ~ _ ~ "(" ~ columnName ~ ")" ~ _ ~
        tableName ~ "(" ~ extColumn ~ ")" ~ _ =>
          ForeignKey(keyName, columnName, tableName, extColumn)
    }

  def constraint = (uniqueOrPk | fk)

  def tableMetaInfo = """(?i)ENGINE=""".r ~ engine ~ """(?i)DEFAULT CHARSET=""".r ~ charset

  def createTable = ("""(?i)CREATE TABLE""".r) ~ ("""(?i)IF NOT EXISTS""".r?) ~ tableName ~
    "(" ~ (column*) ~ (constraint*) ~ ")" ~ (tableMetaInfo?) ^^ {
      case _ ~ _ ~ name ~ "(" ~ columns ~ constraints ~ ")" ~ meta => {
        val columnsData = columns map {
          case colName ~ colType ~ notNull ~ autoInc ~ isDefault ~ _ =>
            Column(cleanString(colName), colType, notNull.isDefined,
                   autoInc.isDefined, isDefault.map(_._2))
        }
        Table(cleanString(name), columnsData.toSet, constraints.toSet)
      }
    }
  def dropTable = "(?i)DROP TABLE" ~ tableName
  def statement = (createTable | dropTable) ~ statementTermination ^^ {
    case res ~ _ => res
  }
  def program   = statement*

  def parse(sql: String) = parseAll(program, sql) map (_.toSet)
}
