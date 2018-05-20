package calculator

sealed abstract class Expr
final case class Literal(v: Double) extends Expr
final case class Ref(name: String) extends Expr
final case class Plus(a: Expr, b: Expr) extends Expr
final case class Minus(a: Expr, b: Expr) extends Expr
final case class Times(a: Expr, b: Expr) extends Expr
final case class Divide(a: Expr, b: Expr) extends Expr

object Calculator {
  def computeValues(namedExpressions: Map[String, Signal[Expr]]): Map[String, Signal[Double]] = {
    namedExpressions.map(n => {
      (n._1, Signal { eval(n._2(), namedExpressions) })
    })
  }

  def eval(expr: Expr, references: Map[String, Signal[Expr]]): Double = {
    expr match {
      case e:Literal => e.v
      case e:Ref => {
        val expr = getReferenceExpr(e.name, references)
        eval(expr, references - e.name)
      }
      case e:Plus => eval(e.a, references) + eval(e.b, references)
      case e:Minus => eval(e.a, references) - eval(e.b, references)
      case e:Times => eval(e.a, references) * eval(e.b, references)
      case e:Divide => eval(e.a, references) / eval(e.b, references)
    }
  }

  /** Get the Expr for a referenced variables.
   *  If the variable is not known, returns a literal NaN.
   */
  private def getReferenceExpr(name: String, references: Map[String, Signal[Expr]]) = {
    references.get(name).fold[Expr] {
      Literal(Double.NaN)
    } { exprSignal =>
      exprSignal()
    }
  }
}

// a = b
// b = c
// c = 10
// a = b, b = a
