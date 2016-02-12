import scala.io._
import cs162.assign3.syntax._
import Aliases._
import scala.io.Source.fromFile

//—————————————————————————————————————————————————————————————————————————
// Main entry point

object Checker {
  type TypeEnv = scala.collection.immutable.HashMap[Var, Type]
  object Illtyped extends Exception

  var typeDefs = Set[TypeDef]()

  def main(args: Array[String]) {
    val filename = args(0)

    val input = fromFile(filename).mkString
    Parsers.program.run(input, filename) match {
      case Left(e) => println(e)
      case Right(program) =>
        val prettied = Pretty.prettySyntax(program)
        typeDefs = program.typedefs

        try {
          getType(program.e, new TypeEnv())
          println("This program is well-typed:\n")
          println(Pretty.prettySyntax(program))
        } catch { case Illtyped => println("This program is ill-typed") }
    }
  }

  // Gets all the constructors associated with a given type name.
  // For example, consider the following typedefs:
  //
  // type Either = Left num | Right bool
  // type Maybe = Some num | None
  //
  // With respect to the above typedefs, `constructors` will return
  // the following underneath the given arguments:
  //
  // constructors(Label("Either")) = Map(Label("Left") -> NumT, Label("Right") -> BoolT)
  // constructors(Label("Maybe")) = Map(Label("Some") -> NumT, Label("None") -> UnitT)
  // constructors(Label("Fake")) throws Illtyped
  //
  def constructors(name: Label): Map[Label, Type] =
    typeDefs.find(_.name == name).map(_.constructors).getOrElse(throw Illtyped)

  // Gets the type of the constructor.
  // For example, considering the typedefs given in the `constructors` comment above,
  // `typename` will return the following with the given arguments:
  //
  // typename(Label("Left")) = Label("Either")
  // typename(Label("Right")) = Label("Either")
  // typename(Label("Some")) = Label("Maybe")
  // typename(Label("None")) = Label("Maybe")
  //
  def typename(constructor: Label): Label =
    typeDefs.find(_.constructors.contains(constructor)).getOrElse(throw Illtyped).name

  def getType(e: Exp, env: TypeEnv): Type =
    e match {
      // variables
      case x: Var => (env.get(x)).getOrElse(throw Illtyped) // FILL ME IN

      // numeric literals
      case _: Num => NumT // FILL ME IN

      // boolean literals
      case _: Bool => BoolT // FILL ME IN

      // `nil` - the literal for unit
      case _: NilExp => UnitT // FILL ME IN

      // builtin arithmetic operators
      case Plus | Minus | Times | Divide => FunT(List(NumT, NumT), NumT) // FILL ME IN

      // builtin relational operators
      case LT | EQ => FunT(List(NumT, NumT), BoolT) // FILL ME IN

      // builtin logical operators
      case And | Or => FunT(List(BoolT, BoolT), BoolT) // FILL ME IN

      // builtin logical operators
      case Not => FunT(List(BoolT), BoolT) // FILL ME IN

      // function creation
      case Fun(params, body) => {
        getType(body, env ++ params.map(i => i._1 -> i._2).toMap)
      } // FILL ME IN

      // function call
      case Call(fun, args) => {
        val x = getType(fun, env)
        x match {
          case y: FunT => {
            if (y.params.length == args.length) {
              for (i <- 0 to y.params.length) {
                if (y.params.apply(i) != getType(args.apply(i), env)) {
                  throw Illtyped
                }
              }
              y.ret
            } else {
              throw Illtyped
            }
          }
          case _ => throw Illtyped
        }
      } // FILL ME IN

      // conditionals 
      case If(e1, e2, e3) => {
        if (getType(e1, env) == BoolT) {
          if (getType(e2, env) == getType(e3, env)) {
            getType(e2, env)
          } else {
            throw Illtyped
          }
        } else {
          throw Illtyped
        }
      } // FILL ME IN

      // let binding
      case Let(x, e1, e2) => getType(e2, env + (x -> getType(e1, env))) // FILL ME IN

      // recursive binding
      case Rec(x, t1, e1, e2) => {
        if (getType(e1, env + (x -> t1)) == t1) {
          getType(e2, env + (x -> getType(e2, env + (x -> t1))))
        } else {
          throw Illtyped
        }
      } // FILL ME IN

      // record literals
      case Record(fields) => RcdT(fields.map { case (k, v) => (k, getType(v, env)) }) // FILL ME IN

      // record access
      case Access(e, field) => {
        e match {
          case Record(fields) => {
            if (fields.contains(field)) {
              getType(fields.getOrElse(field, throw Illtyped), env)
            } else { throw Illtyped }
          }
          case _ => throw Illtyped
        }
      } // FILL ME IN

      // constructor use
      case Construct(constructor, e) => {
        if (constructors(typename(constructor)).getOrElse(constructor, throw Illtyped) == getType(e, env)) {
          getType(e, env)
        } else {
          throw Illtyped
        }
      } // FILL ME IN

      // pattern matching (case ... of ...)
      case Match(e, cases) => {
        e match {
          case x: Var => {
            if (constructors(typename(x.name)).size == cases.length) {
              for (a <- cases; b <- cases) {
                if (getType(a._3, env + (a._2 -> constructors(typename(x.name)).getOrElse(a._1, throw Illtyped)))
                  != getType(b._3, env + (b._2 -> constructors(typename(x.name)).getOrElse(b._1, throw Illtyped)))) {
                  throw Illtyped
                }
              }
              getType(cases.apply(0)._3, env + (cases.apply(0)._2 -> constructors(typename(x.name)).getOrElse(cases.apply(0)._1, throw Illtyped)))
            } else {
              throw Illtyped
            }
          }
          case _ => throw Illtyped
        }
      } // FILL ME IN

    }
}
