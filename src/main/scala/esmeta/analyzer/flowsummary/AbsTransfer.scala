package esmeta.analyzer.flowsummary

import esmeta.cfg.{util => _, *}
import esmeta.flowsummary.{Path => APath, *}
import esmeta.flowsummary.Key.*
import esmeta.flowsummary.Lit.*
import esmeta.flowsummary.Semantics.*
import esmeta.ir.{Func => _, util => _, *}

trait AbsTransferDecl { analyzer: FlowSummaryAnalyzer =>

  /** abstract transfer function */
  class AbsTransfer extends AbsTransferLike {
    import monad.*

    // =========================================================================
    // control points
    // =========================================================================
    /** transfer function for node points */
    def apply(np: NodePoint[?]): Unit =
      if (iter > maxIter) throw BudgetExceeded
      val st = getResult(np)
      given NodePoint[?] = np
      resetSites(np)
      np.node match
        case Block(_, insts, next) =>
          val newSt = insts.foldLeft(st) { (cur, inst) =>
            if (cur.reachable) transferInst(inst, cur) else cur
          }
          next.foreach(to => analyzer += np.copy(node = to) -> newSt)
        case call: Call => transferCall(NodePoint(np.func, call, np.view), st)
        case br: Branch =>
          val c = eval(br.cond)(using ctx(st))
          val (t, e) = refine(br.cond, st.locals)
          if (c.canBeTrue)
            br.thenNode.foreach(to => analyzer += np.copy(node = to) -> AbsState(t))
          if (c.canBeFalse)
            br.elseNode.foreach(to => analyzer += np.copy(node = to) -> AbsState(e))

    /** transfer function for return points */
    def apply(rp: ReturnPoint): Unit =
      for (callerNp <- retEdges.getOrElse(rp, Set()).toList.sorted(using cpOrder))
        propagate(rp, callerNp)

    /** propagate the result of a callee to a caller */
    def propagate(rp: ReturnPoint, callerNp: NodePoint[Call]): Unit =
      for {
        AbsRet(ret) <- rpMap.get(rp)
        next <- callerNp.node.next
        callerSt <- callInfo.get(callerNp)
      } {
        val v = stripKeys(ret)
        analyzer += NodePoint(callerNp.func, next, callerNp.view) ->
        callerSt.define(localName(callerNp.node.lhs), v)
      }

    /** allocation sites: the node and the order of allocation in it */
    private var siteNode: String = ""
    private var siteCount: Int = 0
    def resetSites(np: NodePoint[?]): Unit =
      siteNode = s"${np.func.name}@${np.node.id}"; siteCount = 0
    def nextSite(): String = { siteCount += 1; s"$siteNode#$siteCount" }

    /** the context of an expression in a state */
    def ctx(st: AbsState)(using np: NodePoint[?]): Ctx =
      Ctx(st.locals, np.func.name)

    // =========================================================================
    // calls
    // =========================================================================
    /** the call being analyzed, to dispatch calls to closures in models */
    private var curCall: Option[(NodePoint[Call], AbsState)] = None

    def transferCall(callerNp: NodePoint[Call], st: AbsState): Unit =
      given NodePoint[?] = callerNp
      given Ctx = ctx(st)
      val call = callerNp.node
      val site = s"${callerNp.func.name}@${call.id}"
      val result: AbsVal = call.callInst match
        case ICall(_, fexpr, args) =>
          val fv = eval(fexpr)
          val avs = args.map(eval)
          curCall = Some((callerNp, st))
          var res = AbsVal.Bot
          for (clo <- fv.clos)
            model(clo.fname, avs, site, args, callerNp.func.name) match
              case Some(v) => res = res ⊔ v
              case None =>
                doCall(Some((callerNp, st)), clo.fname, avs, clo.captured)
          if (fv.clos.isEmpty) fexpr match
            case ERef(Field(_, EStr("OwnPropertyKeys"))) =>
              val keys = alloc(site)
              for (l <- keys.locs) heapWrite(l, KAny, AbsVal(LOpaque))
              res = keys
            case _ =>
              warn(s"call to a non-closure in ${callerNp.func.name}: $fexpr")
              res = AbsVal.Clean
          curCall = None
          res
        case ISdoCall(_, base, _, args) =>
          // results of syntax-directed operations derive from the source text
          args.foreach(eval)
          toStr(eval(base))
      if (!result.isBottom) for (next <- call.next)
        analyzer += callerNp.copy(node = next) ->
        st.define(localName(call.lhs), result)

    /** call a function through its CFG */
    def doCall(
      caller: Option[(NodePoint[Call], AbsState)],
      fname: String,
      args: List[AbsVal],
      captured: Locals,
    ): Unit = cfg.fnameMap.get(fname) match
      case None => warn(s"unknown function: $fname")
      case Some(callee) =>
        val view = contextOf(fname, args, captured)
        val (vs, cap) = view match
          case View.Context(vs, cap) => (vs, cap)
          case _ => (args.map(stripKeys(_)), captured.map((k, v) => k -> stripKeys(v)))
        val params = callee.params.zip(vs).map((p, a) => p.lhs.name -> a)
        analyzer += NodePoint(callee, callee.entry, view) -> AbsState(cap ++ params)
        for ((callerNp, callerSt) <- caller)
          callInfo += callerNp -> callerSt
          val rp = ReturnPoint(callee, view)
          retEdges += rp -> (retEdges.getOrElse(rp, Set()) + callerNp)
          propagate(rp, callerNp)

    /** limit the number of calling contexts per function */
    def contextOf(fname: String, args: List[AbsVal], captured: Locals): View =
      // key tags are local to the function that reads the property
      val view = View.Context(
        args.map(stripKeys(_)),
        captured.map((k, v) => k -> stripKeys(v)),
      )
      val views = contexts.getOrElse(fname, Set())
      if (views.contains(view)) view
      else if (views.size < maxContexts) { contexts(fname) = views + view; view }
      else View.Merged

    /** record a return value */
    def doReturn(v: AbsVal)(using np: NodePoint[?]): Unit =
      val rp = ReturnPoint(np.func, np.view)
      val old = getResult(rp)
      val nv = AbsRet(old.ret ⊔ v)
      if (!v.isBottom && nv != old)
        rpMap += rp -> nv
        worklist += rp

    // =========================================================================
    // heap and flows
    // =========================================================================
    /** read an abstract object, registering the current control point */
    def heapObj(l: Loc): Map[Key, AbsVal] =
      for (cp <- curCp) readers(l) = readers.getOrElse(l, Set()) + cp
      heap.getOrElse(l, Map())

    def heapRead(l: Loc, k: Key): AbsVal =
      heapObj(l).getOrElse(k, AbsVal.Bot)

    def heapWrite(l: Loc, k: Key, v: AbsVal): Unit =
      val obj = heap.getOrElse(l, Map())
      val old = obj.getOrElse(k, AbsVal.Bot)
      val nv = old ⊔ stripKeys(v.present, dropWithout = false)
      if (nv != old)
        heap(l) = obj + (k -> nv)
        readers.getOrElse(l, Set()).toList.sorted(using cpOrder).foreach(worklist += _)

    def addFlow(flow: Flow): Unit =
      if (flow.input != flow.output) flows += flow

    /** record that `v` is stored at `out` (an output path) */
    def flowsTo(v: AbsVal, out: APath, depth: Int): Unit =
      for (s <- v.srcs) addFlow(Flow(s.path, out, s.value))
      for (r <- v.rest) addFlow(Flow(APath(Root.Rest(r)), out, true))
      if (depth < maxDepth) for {
        l <- v.locs
        case (KProp(c), cv) <- heapObj(l)
      } flowsTo(cv, if (c == WithElem) out else out.add(c), depth + 1)

    /** list elements of an abstract IR list */
    def listElems(v: AbsVal): AbsVal =
      var res = AbsVal.Bot
      for (r <- v.rest) res = res ⊔ AbsVal.src(APath(Root.Rest(r)))
      for {
        l <- v.locs
        (k, ev) <- heapObj(l)
        if k == KAny || k.isInstanceOf[KIdx]
      } res = res ⊔ ev
      res

    /** the `i`-th element of an abstract IR list */
    def listElem(v: AbsVal, i: Int): AbsVal =
      var res = AbsVal.Bot
      for (r <- v.rest) res = res ⊔ AbsVal.src(APath(Root.Arg(r + i)))
      if (v.popped == AbsVal.Unknown || v.popped == AbsVal.Empty)
        res = res ⊔ listElems(v.copy(rest = Set()))
      else
        for (l <- v.locs)
          res = res ⊔ heapRead(l, KIdx(v.popped + i)) ⊔ heapRead(l, KAny)
      res

    // =========================================================================
    // JS object operations
    // =========================================================================
    /** CodeQL content for a property key */
    def contentOf(key: AbsVal): String = key.cst match
      case Cst.One(LSym(s))               => s"Member[@@$s]"
      case Cst.One(LOpaque)               => "AnyMember"
      case Cst.One(LStr(s)) if isIndex(s) => s"ArrayElement[$s]"
      case Cst.One(LStr(s))               => s"Member[$s]"
      case _ if key.srcs.nonEmpty         => "AnyMember"
      // keys computed only from internal values are array indices
      case _ => "ArrayElement"

    /** JS properties of an abstract object, with index-preserving copies seen
      * as plain array elements
      */
    def props(l: Loc): Iterable[(String, AbsVal)] =
      for (case (KProp(c), v) <- heapObj(l)) yield
        if (c != WithElem) (c, v)
        else
          val srcs = v.srcs.map(s => s.copy(path = s.path.init.add("ArrayElement")))
          ("ArrayElement", v.copy(srcs = srcs))

    /** `key` identifies the key expression, to detect index-preserving copies */
    def readProp(
      obj: AbsVal,
      content: String,
      key: Option[String] = None,
    ): AbsVal =
      var res = AbsVal.Bot
      for (s <- obj.srcs)
        if (!s.value) res = res ⊔ AbsVal(srcs = Set(s), cst = Cst.Top)
        else if (s.path.last == Some(WithoutElem))
          // a value known not to be an array has no array elements
          if (!content.startsWith("ArrayElement"))
            res = res ⊔ AbsVal.src(s.path.init.add(content))
        else if (s.path.depth < maxDepth)
          val k = if (content == "ArrayElement") key else None
          val src = Src(s.path.add(content), true, k)
          res = res ⊔ AbsVal(srcs = Set(src), cst = Cst.Top)
      for (l <- obj.locs; (c, v) <- props(l) if mayAlias(c, content))
        res = res ⊔ v
      if (res.isBottom) AbsVal.Clean else res.copy(cst = Cst.Top)

    def writeProp(
      obj: AbsVal,
      content: String,
      v: AbsVal,
      key: Option[String] = None,
    ): Unit =
      // elements read and written with the same key keep their indices
      val (kept, others) = v.srcs.partition { s =>
        content == "ArrayElement" && key.isDefined && s.value &&
        s.key == key && s.path.last == Some("ArrayElement")
      }
      val rest = v.copy(srcs = others)
      val withSrcs = kept.map(s => Src(s.path.init.add(WithElem), true))
      for (s <- obj.srcs if s.value && s.path.depth < maxDepth)
        flowsTo(rest, s.path.add(content), s.path.depth + 1)
        for (w <- withSrcs) addFlow(Flow(w.path, s.path, true))
      for (l <- obj.locs)
        heapWrite(l, KProp(content), rest)
        if (withSrcs.nonEmpty)
          heapWrite(l, KProp(WithElem), AbsVal(srcs = withSrcs, cst = Cst.Top))

    def alloc(site: String): AbsVal =
      AbsVal(locs = Set(Loc(site)), cst = Cst.Top)

    // =========================================================================
    // models of abstract operations
    // =========================================================================
    def model(
      fname: String,
      args: List[AbsVal],
      site: String,
      argExprs: List[Expr],
      caller: String,
    ): Option[AbsVal] =
      def arg(i: Int): AbsVal = args.lift(i).getOrElse(AbsVal(LUndef))
      // the key variable of a property access, if any
      def keyTag(i: Int): Option[String] = argExprs.lift(i).collect {
        case ERef(x: Local) => s"$caller:${localName(x)}"
      }
      Some(fname match
        case "NormalCompletion" | "Completion" | "ToObject" |
            "RequireObjectCoercible" =>
          arg(0)
        case "IteratorClose" | "AsyncIteratorClose" => arg(1)
        // `Await` resumes through a continuation and never returns normally;
        // approximate the awaited value with the value itself
        case "Await" => arg(0)
        // iteration over an iterable yields its array elements
        case "GetIterator" | "GetIteratorFromMethod" |
            "GetIteratorFlattenable" =>
          val iter = alloc(site)
          for (l <- iter.locs)
            heapWrite(l, KField(iterElem), readProp(arg(0), "ArrayElement"))
          iter
        case "IteratorStep" => arg(0)
        case "IteratorStepValue" | "IteratorValue" =>
          val v = arg(0).locs.foldLeft(AbsVal.Bot)(_ ⊔ heapRead(_, KField(iterElem)))
          if (v.isBottom) AbsVal.Clean else v
        case "GetMethod"             => AbsVal.Clean
        case "CreateBuiltinFunction" => arg(0)
        case "ThrowCompletion"       => AbsVal.Bot
        case "ToString" | "ToPrimitive" | "ToPropertyKey" => toStr(arg(0))
        case "IsArray" =>
          val tested = argExprs.headOption.collect {
            case ERef(x: Local) => localName(x)
          }
          AbsVal(cst = Cst.Top, isArrayOf = tested.toSet)
        case _ if cleanOps.contains(fname) => AbsVal.Clean
        case "Get" | "GetV" =>
          readProp(arg(0), contentOf(arg(1)), keyTag(1))
        case "Set" | "CreateDataProperty" | "CreateDataPropertyOrThrow" =>
          writeProp(arg(0), contentOf(arg(1)), arg(2), keyTag(1)); AbsVal.Clean
        case "DefinePropertyOrThrow" | "DefineProperty" =>
          val desc =
            arg(2).locs.foldLeft(AbsVal.Bot)(_ ⊔ heapRead(_, KField("Value")))
          writeProp(arg(0), contentOf(arg(1)), desc); AbsVal.Clean
        case "ArraySpeciesCreate" | "ArrayCreate" | "OrdinaryObjectCreate" |
            "TypedArraySpeciesCreate" | "TypedArrayCreateSameType" =>
          alloc(site)
        case "CreateArrayFromList" =>
          val arr = alloc(site)
          writeProp(arr, "ArrayElement", listElems(arg(0)))
          arr
        case "CreateListFromArrayLike" =>
          val list = alloc(site)
          for (l <- list.locs)
            heapWrite(l, KAny, readProp(arg(0), "ArrayElement"))
          list
        case "Call" =>
          callJS(arg(0), arg(1), args.lift(2).getOrElse(AbsVal.Bot))
        case "Construct" => alloc(site)
        case "Invoke" =>
          val f = readProp(arg(0), contentOf(arg(1)))
          callJS(f, arg(0), args.lift(2).getOrElse(AbsVal.Bot))
        case _ => return None
      )

    def toStr(v: AbsVal): AbsVal = v.cst match
      case Cst.One(lit) if v.srcs.isEmpty && v.locs.isEmpty =>
        litToStr(lit).fold(AbsVal.Clean)(s => AbsVal(LStr(s)))
      case _ =>
        val contents = for { l <- v.locs; (_, cv) <- props(l) } yield cv
        val all = contents.foldLeft(v)(_ ⊔ _)
        AbsVal(srcs = all.srcs.map(_.toTaint), cst = Cst.Top)

    /** a call to a JS function value */
    def callJS(f: AbsVal, thisV: AbsVal, argList: AbsVal): AbsVal =
      var res = AbsVal.Bot
      for (s <- f.srcs if s.value && s.path.depth < maxDepth)
        val fp = s.path
        flowsTo(thisV, fp.add("Parameter[this]"), fp.depth + 1)
        for (i <- 0 until maxParams)
          flowsTo(listElem(argList, i), fp.add(s"Parameter[$i]"), fp.depth + 1)
        res = res ⊔ AbsVal.src(fp.add("ReturnValue"))
      // built-in functions created from abstract closures
      for (clo <- f.clos)
        val args = List(thisV, argList, AbsVal(LUndef))
        doCall(curCall, clo.fname, args, clo.captured)
      if (res.isBottom && f.clos.isEmpty) AbsVal.Clean else res

    // =========================================================================
    // instructions
    // =========================================================================
    /** transfer function for normal instructions */
    def transfer(inst: NormalInst)(using np: NodePoint[?]): Updater =
      st => transferInst(inst, st)

    def transferInst(inst: NormalInst, st: AbsState)(using
      np: NodePoint[?],
    ): AbsState =
      given Ctx = ctx(st)
      val locals = st.locals
      inst match
        case IExpr(EYet(msg)) => yetAssign(msg, st)
        case IExpr(expr)      => eval(expr); st
        case ILet(x, expr)    => st.define(x.name, eval(expr))
        case IAssign(ref, expr) => assign(ref, eval(expr), st)
        case IAssert(cond) =>
          AbsState(locals ++ compChecked(cond).map(compMark(_) -> AbsVal.Clean))
        case IExpand(_, _) | IDelete(_, _) | IPrint(_) | INop() => st
        case IPush(elem, list, _) =>
          val v = eval(elem)
          val lv = eval(list)
          for (l <- lv.locs) heapWrite(l, KAny, v)
          (list, lv.rest.nonEmpty) match
            // materialize the remaining arguments as a list
            case (ERef(x: Local), true) =>
              val nl = alloc(nextSite())
              for (l <- nl.locs) heapWrite(l, KAny, listElems(lv) ⊔ v)
              val nv = lv.copy(
                rest = Set(),
                locs = lv.locs ++ nl.locs,
                popped = AbsVal.Unknown,
              )
              AbsState(locals + (localName(x) -> nv))
            case _ => st
        case IPop(lhs, list, front) =>
          val lv = eval(list)
          val elem = if (front) listElem(lv, 0) else listElems(lv)
          val st1 = locals + (localName(lhs) -> elem)
          // popping from the front of `ArgumentsList` shifts its offset
          list match
            case ERef(x: Local) if front =>
              val popped =
                if (lv.popped < 0 || lv.popped == AbsVal.Empty) lv.popped
                else lv.popped + 1
              val shifted = lv.copy(rest = lv.rest.map(_ + 1), popped = popped)
              AbsState(st1 + (localName(x) -> shifted))
            case _ => AbsState(st1)
        case IReturn(expr) =>
          doReturn(eval(expr)); AbsState.Bot

    def assign(ref: Ref, v: AbsVal, st: AbsState)(using Ctx): AbsState =
      ref match
        case x: Local => st.define(localName(x), v)
        case Global(_) => st
        case Field(base, kexpr) =>
          val bv = evalRef(base)
          val k = eval(kexpr)
          for (l <- bv.locs) heapWrite(l, fieldKey(k), v)
          st

    // =========================================================================
    // `yet` steps: approximate with the variables they mention
    // =========================================================================
    def yetAssign(msg: String, st: AbsState)(using ctx: Ctx): AbsState =
      yets += msg
      val locals = st.locals
      val vars = varPattern.findAllMatchIn(msg).map(_.group(1)).toList
      // "... calls to _F_ ...": call the closure with elements of the lists
      for {
        m <- callsToPattern.findAllMatchIn(msg)
        f <- locals.get(m.group(1)).toList
        clo <- f.clos
      } {
        val elems = vars
          .filter(_ != m.group(1))
          .flatMap(locals.get)
          .foldLeft(AbsVal.Bot)((acc, v) => acc ⊔ listElems(v))
        doCall(None, clo.fname, List(elems, elems), clo.captured)
      }
      assignPattern.findFirstMatchIn(msg) match
        case Some(m) =>
          val x = m.group(2)
          AbsState(locals + (x -> yetValue(vars.filter(_ != x), locals)))
        case None => st

    def yetValue(vars: List[String], locals: Locals): AbsVal =
      vars.flatMap(locals.get).foldLeft(AbsVal.Clean)((acc, v) => acc ⊔ toStr(v))

    // =========================================================================
    // expressions
    // =========================================================================
    /** transfer function for expressions */
    def transfer(expr: Expr)(using np: NodePoint[Node]): Result[AbsValue] =
      st => (AbsValue(eval(expr)(using ctx(st))), st)

    def transfer(st: AbsState, unary: EUnary, operand: AbsValue)(using
      np: NodePoint[Node],
    ): AbsValue = AbsValue(eval(unary)(using ctx(st)))

    def transfer(
      st: AbsState,
      binary: EBinary,
      left: AbsValue,
      right: AbsValue,
    )(using np: NodePoint[Node]): AbsValue =
      AbsValue(this.binary(binary.bop, left.v, right.v))

    def transfer(st: AbsState, vop: VOp, vs: List[AbsValue])(using
      np: NodePoint[Node],
    ): AbsValue = vop match
      case VOp.Concat => AbsValue(vs.foldLeft(AbsVal.Clean)((a, v) => a ⊔ toStr(v.v)))
      case _          => AbsValue.Top

    def transfer(st: AbsState, mop: MOp, vs: List[AbsValue])(using
      np: NodePoint[Node],
    ): AbsValue = AbsValue.Top

    def eval(expr: Expr)(using ctx: Ctx): AbsVal = expr match
      case EStr(s)        => AbsVal(LStr(s))
      case ENumber(d)     => AbsVal(LNum(d))
      case EMath(n)       => AbsVal(LMath(n))
      case EInfinity(pos) => AbsVal(LInf(pos))
      case EBool(b)       => AbsVal(LBool(b))
      case EUndef()       => AbsVal(LUndef)
      case ENull()        => AbsVal(LNull)
      case EEnum(name)    => AbsVal(LEnum(name))
      case _: LiteralExpr => AbsVal.Clean
      case ERef(ref)      => evalRef(ref)
      case EUnary(UOp.Not, e) =>
        val v = eval(e); AbsVal.bool(v.canBeFalse, v.canBeTrue)
      case EUnary(UOp.Neg, e) => numOp(eval(e), eval(e), (a, _) => -a)
      case EUnary(_, _)       => AbsVal.Clean
      case EBinary(op, l, r)  => binary(op, eval(l), eval(r))
      case EVariadic(VOp.Concat, es) =>
        val vs = es.map(eval)
        val strs = vs.map(_.cst).collect { case Cst.One(LStr(s)) => s }
        if (strs.length == vs.length) AbsVal(LStr(strs.mkString))
        else vs.foldLeft(AbsVal.Clean)((acc, v) => acc ⊔ toStr(v))
      case EVariadic(_, es) => es.foreach(eval); AbsVal.Clean
      case ESubstring(e, from, to) =>
        eval(from); to.foreach(eval); toStr(eval(e))
      case ETrim(e, _)                 => toStr(eval(e))
      case EConvert(COp.ToStr(_), e)   => toStr(eval(e))
      case EConvert(COp.ToCodeUnit, e) => toStr(eval(e))
      case EConvert(cop, e) =>
        val v = eval(e)
        v.cst match
          case Cst.One(lit) if v.srcs.isEmpty =>
            (cop, lit.toDouble) match
              case (COp.ToNumber | COp.ToApproxNumber, Some(d)) =>
                AbsVal(LNum(d))
              case (COp.ToMath, Some(d)) if !d.isInfinite =>
                AbsVal(LMath(BigDecimal(d)))
              case _ => AbsVal.Clean
          case _ => AbsVal.Clean
      case EExists(x: Local) =>
        ctx.st.get(localName(x)) match
          case None    => AbsVal(LBool(false))
          case Some(v) => AbsVal.bool(true, v.maybeAbsent)
      case EExists(_)       => AbsVal.Clean
      case ETypeCheck(e, _) => eval(e); AbsVal.Clean
      case EClo(fname, captured) =>
        val cap =
          captured.map(n => n.name -> ctx.st.getOrElse(n.name, AbsVal.Bot))
        AbsVal(clos = Set(Clo(fname, cap.toMap)), cst = Cst.Top)
      case e: AllocExpr =>
        val site = nextSite()
        val v = alloc(site)
        val l = Loc(site)
        e match
          case ERecord(_, pairs) =>
            for ((f, fe) <- pairs) heapWrite(l, KField(f), eval(fe))
          case EList(es) =>
            for ((fe, i) <- es.zipWithIndex) heapWrite(l, KIdx(i), eval(fe))
          case ECopy(obj) =>
            val ov = eval(obj)
            for (ol <- ov.locs; (k, kv) <- heapObj(ol)) heapWrite(l, k, kv)
            if (ov.rest.nonEmpty) heapWrite(l, KAny, listElems(ov))
          case EMap(_, pairs) =>
            for ((ke, ve) <- pairs) { eval(ke); heapWrite(l, KAny, eval(ve)) }
          case EKeys(_, _) => heapWrite(l, KAny, AbsVal(LOpaque))
        v
      case EParse(code, rule) => eval(rule); toStr(eval(code))
      case EYet(msg) =>
        yets += msg
        val vars = varPattern.findAllMatchIn(msg).map(_.group(1)).toList
        yetValue(vars, ctx.st)
      case _ => AbsVal.Clean

    def evalRef(ref: Ref)(using ctx: Ctx): AbsVal = ref match
      case x: Local => ctx.st.getOrElse(localName(x), AbsVal.Bot).present
      case Global(g) if g.startsWith("SYMBOL.") =>
        AbsVal(LSym(g.stripPrefix("SYMBOL.")))
      case Global(_) => AbsVal.Clean
      case Field(base, kexpr) =>
        val bv = evalRef(base)
        val k = eval(kexpr)
        k.cst match
          // completion records are not represented, but other records may
          // have a `Value` field
          case Cst.One(LStr("Value")) if isComp(base) => bv
          case Cst.One(LStr("Value")) =>
            bv.locs.foldLeft(bv.copy(locs = Set())) { (acc, l) =>
              heapObj(l).get(KField("Value")) match
                case Some(v) => acc ⊔ v
                case None    => acc ⊔ AbsVal(locs = Set(l), cst = Cst.Top)
            }
          case _ =>
            var res = AbsVal.Bot
            if (bv.rest.nonEmpty) res = res ⊔ (k.cst match
              case Cst.One(lit) =>
                lit.toDouble.fold(AbsVal.Bot)(d => listElem(bv, d.toInt))
              case _ => listElems(bv)
            )
            for (s <- bv.srcs)
              res = res ⊔ AbsVal(srcs = Set(s.toTaint), cst = Cst.Top)
            for (l <- bv.locs)
              val obj = heapObj(l)
              fieldKey(k) match
                case KAny =>
                  for ((k, v) <- obj if k == KAny || k.isInstanceOf[KIdx])
                    res = res ⊔ v
                case kk @ KIdx(_) =>
                  res = res ⊔ obj.getOrElse(kk, AbsVal.Bot) ⊔
                    obj.getOrElse(KAny, AbsVal.Bot)
                case kk => res = res ⊔ obj.getOrElse(kk, AbsVal.Bot)
            if (res.isBottom) AbsVal.Clean else res

    def fieldKey(k: AbsVal): Key = k.cst match
      case Cst.One(LStr(s)) => KField(s)
      case Cst.One(lit)     => lit.toDouble.fold(KAny)(d => KIdx(d.toInt))
      case _                => KAny

    def binary(op: BOp, l: AbsVal, r: AbsVal): AbsVal = op match
      case BOp.Eq | BOp.Equal =>
        (l.cst, r.cst) match
          case (Cst.One(a), Cst.One(b)) if l.srcs.isEmpty && r.srcs.isEmpty =>
            val eq = (a.toDouble, b.toDouble) match
              case (Some(x), Some(y)) => x == y
              case _                  => a == b
            AbsVal(LBool(eq))
          case _ => AbsVal.Clean
      case BOp.Lt =>
        (l.cst, r.cst) match
          case (Cst.One(a), Cst.One(b)) =>
            (a.toDouble, b.toDouble) match
              case (Some(x), Some(y)) => AbsVal(LBool(x < y))
              case _                  => AbsVal.Clean
          case _ => AbsVal.Clean
      case BOp.And =>
        AbsVal.bool(l.canBeTrue && r.canBeTrue, l.canBeFalse || r.canBeFalse)
      case BOp.Or =>
        AbsVal.bool(l.canBeTrue || r.canBeTrue, l.canBeFalse && r.canBeFalse)
      case BOp.Add => numOp(l, r, _ + _)
      case BOp.Sub => numOp(l, r, _ - _)
      case BOp.Mul => numOp(l, r, _ * _)
      case _       => AbsVal.Clean

    def numOp(l: AbsVal, r: AbsVal, f: (Double, Double) => Double): AbsVal =
      (l.cst, r.cst) match
        case (Cst.One(a), Cst.One(b)) =>
          (a, b, a.toDouble, b.toDouble) match
            case (_: LNum, _: LNum, Some(x), Some(y)) => AbsVal(LNum(f(x, y)))
            case (_, _, Some(x), Some(y)) =>
              val z = f(x, y)
              if (z.isInfinite) AbsVal(LInf(z > 0))
              else AbsVal(LMath(BigDecimal(z)))
            case _ => AbsVal.Clean
        case _ => AbsVal.Clean
  }
}
