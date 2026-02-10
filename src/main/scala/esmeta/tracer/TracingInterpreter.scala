package esmeta.tracer

import esmeta.cfg.*
import esmeta.error.NotSupported
import esmeta.es.builtin.INTRINSICS
import esmeta.interpreter.Interpreter
import esmeta.ir.*
import esmeta.state.*
import esmeta.ty.*
import esmeta.util.BaseUtils.raise
import scala.util.Try
import java.io.PrintWriter
import esmeta.util.Loc

class TracingInterpreter(
  st: State,
  analysis: Analysis,
  log: Boolean = false,
  detail: Boolean = true, // turn off GC
  logPW: Option[PrintWriter] = None,
  timeLimit: Option[Int] = None,
  // TODO print detailed log when noCompare is true
  // noCompare: Boolean = false,
) extends Interpreter(
    st = st,
    tyCheck = false,
    log = log,
    detail = detail,
    logPW = logPW,
    timeLimit = timeLimit,
  ) {
  override def eval(inst: NormalInst): Unit = inst match
    case IPrint(expr) =>
      val v = eval(expr)
      v match
        case Str(str) => analysis.__print(str)
        case _        => raise(s"IPrint with non-string value: $v")
    case ITrace(es) =>
      val traced = es.map(eval)
      given State = st
      traced match
        case Enum("read") :: v :: Nil =>
          analysis.appendWithLoc(Trace.Read(v.asStr))

        case Enum("binarypre" | "binarypre-switch") :: op :: _ =>
          analysis.appendWithLoc(Trace.BinaryPre(op.asStr))

        case Enum("binary" | "binary-switch") :: op :: _ =>
          analysis.appendWithLoc(Trace.Binary(op.asStr))

        case Enum("getfieldpre") :: o :: p :: reciever :: Nil =>
          val globalThis = for {
            ec <- st.get(NamedAddr("EXECUTION_STACK"), Math(0))
            realm <- st.get(ec, Str("Realm"))
            gbl <- st.get(realm, Str("GlobalObject"))
          } yield gbl
          val isInternal = isInternalGet
          analysis.appendWithLoc(
            Trace.GetFieldPre(
              p.indexStr.normalizeFieldName,
              isInternal = Some(isInternal),
              isGlobal = globalThis
                .map(_ == reciever)
                .toOption,
            ),
          )

        case Enum("getfield") :: o :: p :: reciever :: r :: Nil =>
          val globalThis = for {
            ec <- st.get(NamedAddr("EXECUTION_STACK"), Math(0))
            realm <- st.get(ec, Str("Realm"))
            gbl <- st.get(realm, Str("GlobalObject"))
          } yield gbl
          val isInternal = isInternalGet
          analysis.appendWithLoc(
            Trace.GetField(
              p.indexStr.normalizeFieldName,
              isInternal = Some(isInternal),
              isGlobal = globalThis
                .map(_ == reciever)
                .toOption,
            ),
          )

        case Enum("putfield") :: o :: p :: v :: r :: Nil => // TODO reciever?
          analysis.appendWithLoc(Trace.PutField(p.indexStr.normalizeFieldName))

        case Enum("putfieldpre") :: o :: p :: v :: Nil => // TODO reciever?
          analysis.appendWithLoc(
            Trace.PutFieldPre(p.indexStr.normalizeFieldName),
          )

        case Enum("functionenter") :: fo :: args :: Nil =>
          val name = for {
            map <- st.get(fo, Str("__MAP__"))
            desc <- st.get(map, Str("name"))
            name <- st.get(desc, Str("Value"))
            nameStr <- Try(name.asStr)
          } yield nameStr
          analysis.appendWithLoc(Trace.FunctionEnter(name.getOrElse("<error>")))

        case Enum("functionexit") :: fo :: args :: r :: Nil =>
          val name = for {
            map <- st.get(fo, Str("__MAP__"))
            desc <- st.get(map, Str("name"))
            name <- st.get(desc, Str("Value"))
            nameStr <- Try(name.asStr)
          } yield nameStr
          analysis.appendWithLoc(Trace.FunctionExit(name.getOrElse("<error>")))

        case Enum("write") :: name :: v :: Nil =>
          analysis.appendWithLoc(Trace.Write(name.indexStr))

        case Enum("literal") :: v :: Nil =>
          analysis.appendWithLoc(Trace.Literal(v.unpackIfCompletion.esTypeOf))

        case Enum("unarypre") :: op :: _ =>
          analysis.appendWithLoc(Trace.UnaryPre(op.indexStr))

        case Enum("unary") :: op :: _ =>
          analysis.appendWithLoc(Trace.Unary(op.indexStr))

        case Enum("invokefunpre") :: f :: args =>
          val name = for {
            map <- st.get(f, Str("__MAP__"))
            desc <- st.get(map, Str("name"))
            name <- st.get(desc, Str("Value"))
            nameStr <- Try(name.asStr)
          } yield nameStr
          val isInternal = isInternalCall
          analysis.appendWithLoc(
            Trace.InvokeFunPre(
              name.getOrElse("print"), // ad-hoc
              isInternal = Some(isInternal),
            ),
          )

        case Enum("invokefun") :: f :: args =>
          val name = for {
            map <- st.get(f, Str("__MAP__"))
            desc <- st.get(map, Str("name"))
            name <- st.get(desc, Str("Value"))
            nameStr <- Try(name.asStr)
          } yield nameStr
          val isInternal = isInternalCall
          analysis.appendWithLoc(
            Trace.InvokeFun(
              name.getOrElse("print"), // ad-hoc
              isInternal = Some(isInternal),
            ),
          )

        case Enum("declare") :: name :: Nil =>
          analysis.appendWithLoc(Trace.Declare(name.indexStr))

        case Enum("forinobject") :: obj :: Nil =>
          analysis.appendWithLoc(Trace.ForInObject(obj.toString))

        case Enum("conditional") :: _ => // suppress for now
        case Enum(unknown) :: _ =>
          raise(s"unknown trace kind: $unknown")
          analysis.__print(s"trace: ${traced.mkString(", ")}")

        case _ => raise(s"illegal trace format: ${traced.mkString(", ")}")
    case _ => super.eval(inst)

  lazy val trace = { result; TraceLog(analysis.ts.toVector) }

  private def isInternalGet = {
    val getMethodPattern = """Record\[.+\]\.Get""".r
    st.callStack
      .find(c => {
        val n = c.name
        val isFeature = c.context.func.isSDO || c.context.func.isBuiltin
        isFeature || getMethodPattern.matches(n) || n == "GetValue"
      })
      .headOption
      .map(_.name) match {
      case Some("GetValue")                           => false
      case Some(str) if getMethodPattern.matches(str) => true
      case _                                          => true
    }
  }

  private def isInternalCall = {
    st.callStack
      .filter(c => {
        val n = c.name
        val isFeature = c.context.func.isSDO || c.context.func.isBuiltin
        isFeature || n == "Call" || n == "EvaluateCall" || n == "Construct" || n == "EvaluateNew"
      })
      .headOption
      .map(_.name) match {
      case Some("EvaluateCall" | "EvaluateNew") => false
      case Some("Call" | "Construct")           => true
      case _                                    => true
    }
  }

  private def recordCurrentLocation(trace: Trace): Trace = {
    enum LocState:
      case FoundLoc(loc: Loc)
      case NoLoc
    def esLocOpt(ctxt: Context): Option[LocState] = ctxt.astOpt match
      case Some(ast) =>
        Some(ast.loc match
          case Some(loc) => LocState.FoundLoc(loc)
          case None      => LocState.NoLoc,
        )
      case None => None

    val cst = st.context :: st.callStack.map(_.context)
    cst
      .flatMap(esLocOpt)
      .headOption match
      case Some(LocState.FoundLoc(loc)) => trace.setLoc(Some(loc))
      case _                            =>

    trace
  }

  extension (analysis: Analysis) {
    inline def appendWithLoc(trace: Trace)(using st: State): Unit = {
      val traceWithLoc = recordCurrentLocation(trace)
      analysis.append(traceWithLoc)
    }
  }

  extension (str: String) {
    def normalizeFieldName: String =
      // TODO user-defined symbols?
      val symbolPattern = "#INTRINSICS.Symbol."
      if str.startsWith(symbolPattern) then
        val symName = str.stripPrefix(symbolPattern)
        s"Symbol(Symbol.$symName)"
      else str
  }

  extension (value: Value) {
    def toSymbolStringIfPossible(using st: State): String = value match
      case v if SymbolT.contains(v, st) =>
        val desc = for {
          desc <- st.get(v.asAddr, Str("Description"))
        } yield desc
        s"Symbol(${desc.toOption.map(_.asStr).getOrElse("")})"
      case v => v.toString

    def indexStr: String = value match
      case Str(s)    => s
      case Number(n) => n.toString
      // TODO should include spec math and bigint?
      // case _ => raise("value is not string or number")
      case Math(n)    => n.toString
      case BigInt(bi) => bi.toString
      case v          => v.toString

    def esTypeOf(using st: State): String = value match
      case n: Number => "number"
      case b: BigInt => "bigint"
      case s: Str    => "string"
      case b: Bool   => "boolean"
      case Undef     => "undefined"
      case Null      => "object" // yes this is correct
      case v =>
        if (SymbolT.contains(v, st)) "symbol"
        else if (ObjectT.contains(v, st)) {
          if st.get(v.asAddr, Str("Call")).isSuccess then "function"
          else "object"
        } else raise("value is spec-level; should not happen")

  }

}
