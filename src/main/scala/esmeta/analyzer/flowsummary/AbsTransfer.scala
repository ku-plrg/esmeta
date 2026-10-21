package esmeta.analyzer.flowsummary

import esmeta.cfg.{util => _, *}
import esmeta.flowsummary.{Path => APath, *}
import esmeta.flowsummary.Key.*
import esmeta.flowsummary.Lit.*
import esmeta.flowsummary.Semantics.*
import esmeta.ir.{Func => _, util => _, *}

trait AbsTransferDecl { analyzer: FlowSummaryAnalyzer =>

  /** abstract transfer function
    *
    * Expressions never change the abstract state, but they may read or write
    * the heap. Such side effects happen only when a `Result` runs, never when
    * it is built, so they keep the evaluation order of the IR.
    */
  class AbsTransfer extends AbsTransferLike {

    /** loading monads */
    import monad.*

    /** loading predefined abstract values */
    import AbsValue.{Bot, Clean, Empty, Unknown, alloc, bool, src}

    // =========================================================================
    // Implementation for General AbsTransfer
    // =========================================================================
    /** transfer function for node points */
    def apply(np: NodePoint[?]): Unit =
      if (iter > maxIter) throw BudgetExceeded
      val st = getResult(np)
      given NodePoint[?] = np
      resetSites(np)
      np.node match
        case Block(_, insts, next) =>
          val newSt = insts.foldLeft(st) {
            case (nextSt, _) if nextSt.isBottom => nextSt
            case (nextSt, inst)                 => transfer(inst)(nextSt)
          }
          next.foreach(to => analyzer += getNextNp(np, to) -> newSt)
        case call: Call =>
          val (_, newSt) = (for {
            v <- transfer(call)
            _ <-
              if (v.isBottom) put(AbsState.Bot)
              else modify(_.define(call.lhs, v))
          } yield ())(st)
          call.next.foreach(to => analyzer += getNextNp(np, to) -> newSt)
        case br: Branch =>
          (for { v <- transfer(br.cond); st <- get } yield {
            val (thenSt, elseSt) = refine(br.cond, st)
            if (v.canBeTrue)
              br.thenNode.foreach(to => analyzer += getNextNp(np, to) -> thenSt)
            if (v.canBeFalse)
              br.elseNode.foreach(to => analyzer += getNextNp(np, to) -> elseSt)
          })(st)

    /** get next node point */
    def getNextNp(fromCp: NodePoint[Node], to: Node): NodePoint[Node] =
      fromCp.copy(node = to)

    /** transfer function for return points */
    def apply(rp: ReturnPoint): Unit =
      for (
        callerNp <- retEdges.getOrElse(rp, Set()).toList.sorted(using cpOrder)
      ) propagate(rp, callerNp)

    /** propagate the result of a callee to a caller */
    def propagate(rp: ReturnPoint, callerNp: NodePoint[Call]): Unit =
      for {
        AbsRet(value) <- rpMap.get(rp)
        next <- callerNp.node.next
        callerSt <- callInfo.get(callerNp)
      } analyzer += getNextNp(callerNp, next) ->
      callerSt.define(callerNp.node.lhs, value.stripKeys())

    /** allocation sites: the node and the order of allocation in it */
    private var siteNode: String = ""
    private var siteCount: Int = 0
    def resetSites(np: NodePoint[?]): Unit =
      siteNode = s"${np.func.name}@${np.node.id}"; siteCount = 0
    def nextSite(): String = { siteCount += 1; s"$siteNode#$siteCount" }

    // =========================================================================
    // calls
    // =========================================================================
    /** a caller and its abstract state right before the call */
    type Caller = (NodePoint[Call], AbsState)

    /** transfer function for call instructions */
    def transfer(call: Call)(using np: NodePoint[?]): Result[AbsValue] =
      val callerNp = NodePoint(np.func, call, np.view)
      call.callInst match
        case ICall(_, fexpr, args) =>
          for {
            fv <- transfer(fexpr)
            avs <- join(args.map(transfer))
            st <- get
          } yield callClosures((callerNp, st), fexpr, fv, args, avs)
        case ISdoCall(_, base, _, args) =>
          // results of syntax-directed operations derive from the source text
          for {
            _ <- join(args.map(transfer))
            bv <- transfer(base)
          } yield toStr(bv)

    /** call the abstract closures of a function value */
    def callClosures(
      caller: Caller,
      fexpr: Expr,
      fv: AbsValue,
      argExprs: List[Expr],
      args: List[AbsValue],
    ): AbsValue =
      val (callerNp, _) = caller
      val site = s"${callerNp.func.name}@${callerNp.node.id}"
      var res = Bot
      for (clo <- fv.clos)
        model(clo.fname, args, site, argExprs, caller) match
          case Some(v) => res = res ⊔ v
          case None    => doCall(Some(caller), clo.fname, args, clo.captured)
      if (fv.clos.isEmpty) fexpr match
        case ERef(Field(_, EStr("OwnPropertyKeys"))) =>
          val keys = alloc(site)
          for (l <- keys.locs) heapWrite(l, KAny, AbsValue(LOpaque))
          res = keys
        case _ =>
          warn(s"call to a non-closure in ${callerNp.func.name}: $fexpr")
          res = Clean
      res

    /** call a function through its CFG */
    def doCall(
      caller: Option[Caller],
      fname: String,
      args: List[AbsValue],
      captured: Map[Name, AbsValue],
    ): Unit = cfg.fnameMap.get(fname) match
      case None => warn(s"unknown function: $fname")
      case Some(callee) =>
        val view = contextOf(fname, args, captured)
        val (vs, cap) = view match
          case View.Context(vs, cap) => (vs, cap)
          case _ =>
            (
              args.map(_.stripKeys()),
              captured.map((k, v) => k -> v.stripKeys()),
            )
        val params = callee.params.zip(vs).map((p, a) => p.lhs -> a)
        analyzer += NodePoint(callee, callee.entry, view) ->
        AbsState(Map[Local, AbsValue]() ++ cap ++ params)
        for ((callerNp, callerSt) <- caller)
          callInfo += callerNp -> callerSt
          val rp = ReturnPoint(callee, view)
          retEdges += rp -> (retEdges.getOrElse(rp, Set()) + callerNp)
          propagate(rp, callerNp)

    /** limit the number of calling contexts per function */
    def contextOf(
      fname: String,
      args: List[AbsValue],
      captured: Map[Name, AbsValue],
    ): View =
      // key tags are local to the function that reads the property
      val view = View.Context(
        args.map(_.stripKeys()),
        captured.map((k, v) => k -> v.stripKeys()),
      )
      val views = contexts.getOrElse(fname, Set())
      if (views.contains(view)) view
      else if (views.size < maxContexts) {
        contexts(fname) = views + view; view
      } else View.Merged

    /** record a return value */
    def doReturn(v: AbsValue)(using np: NodePoint[?]): Unit =
      val rp = ReturnPoint(np.func, np.view)
      val old = getResult(rp)
      val newRet = old ⊔ AbsRet(v)
      if (!v.isBottom && newRet != old)
        rpMap += rp -> newRet
        worklist += rp

    // =========================================================================
    // heap and flows
    // =========================================================================
    /** read an abstract object, registering the current control point */
    def heapObj(l: Loc): Map[Key, AbsValue] =
      for (cp <- curCp) readers(l) = readers.getOrElse(l, Set()) + cp
      heap.getOrElse(l, Map())

    def heapRead(l: Loc, k: Key): AbsValue =
      heapObj(l).getOrElse(k, Bot)

    def heapWrite(l: Loc, k: Key, v: AbsValue): Unit =
      val obj = heap.getOrElse(l, Map())
      val old = obj.getOrElse(k, Bot)
      val nv = old ⊔ v.present.stripKeys(dropWithout = false)
      if (nv != old)
        heap(l) = obj + (k -> nv)
        readers
          .getOrElse(l, Set())
          .toList
          .sorted(using cpOrder)
          .foreach(worklist += _)

    def addFlow(flow: Flow): Unit =
      if (flow.input != flow.output) flows += flow

    /** record that `v` is stored at `out` (an output path) */
    def flowsTo(v: AbsValue, out: APath, depth: Int): Unit =
      for (s <- v.srcs) addFlow(Flow(s.path, out, s.value))
      for (r <- v.rest) addFlow(Flow(APath(Root.Rest(r)), out, true))
      if (depth < maxDepth) for {
        l <- v.locs
        case (KProp(c), cv) <- heapObj(l)
      } flowsTo(cv, if (c == WithElem) out else out.add(c), depth + 1)

    /** list elements of an abstract IR list */
    def listElems(v: AbsValue): AbsValue =
      var res = Bot
      for (r <- v.rest) res = res ⊔ src(APath(Root.Rest(r)))
      for {
        l <- v.locs
        (k, ev) <- heapObj(l)
        if k == KAny || k.isInstanceOf[KIdx]
      } res = res ⊔ ev
      res

    /** the `i`-th element of an abstract IR list */
    def listElem(v: AbsValue, i: Int): AbsValue =
      var res = Bot
      for (r <- v.rest) res = res ⊔ src(APath(Root.Arg(r + i)))
      if (v.popped == Unknown || v.popped == Empty)
        res = res ⊔ listElems(v.copy(rest = Set()))
      else
        for (l <- v.locs)
          res = res ⊔ heapRead(l, KIdx(v.popped + i)) ⊔ heapRead(l, KAny)
      res

    // =========================================================================
    // JS object operations
    // =========================================================================
    /** CodeQL content for a property key */
    def contentOf(key: AbsValue): String = key.cst match
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
    def props(l: Loc): Iterable[(String, AbsValue)] =
      for (case (KProp(c), v) <- heapObj(l))
        yield
          if (c != WithElem) (c, v)
          else
            val srcs =
              v.srcs.map(s => s.copy(path = s.path.init.add("ArrayElement")))
            ("ArrayElement", v.copy(srcs = srcs))

    /** `key` identifies the key expression, to detect index-preserving copies
      */
    def readProp(
      obj: AbsValue,
      content: String,
      key: Option[String] = None,
    ): AbsValue =
      var res = Bot
      for (s <- obj.srcs)
        if (!s.value) res = res ⊔ AbsValue(srcs = Set(s), cst = Cst.Top)
        else if (s.path.last == Some(WithoutElem))
          // a value known not to be an array has no array elements
          if (!content.startsWith("ArrayElement"))
            res = res ⊔ src(s.path.init.add(content))
        else if (s.path.depth < maxDepth)
          val k = if (content == "ArrayElement") key else None
          val newSrc = Src(s.path.add(content), true, k)
          res = res ⊔ AbsValue(srcs = Set(newSrc), cst = Cst.Top)
      for (l <- obj.locs; (c, v) <- props(l) if mayAlias(c, content))
        res = res ⊔ v
      if (res.isBottom) Clean else res.copy(cst = Cst.Top)

    def writeProp(
      obj: AbsValue,
      content: String,
      v: AbsValue,
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
          heapWrite(
            l,
            KProp(WithElem),
            AbsValue(srcs = withSrcs, cst = Cst.Top),
          )

    // =========================================================================
    // models of abstract operations
    // =========================================================================
    def model(
      fname: String,
      args: List[AbsValue],
      site: String,
      argExprs: List[Expr],
      caller: Caller,
    ): Option[AbsValue] =
      val callerName = caller._1.func.name
      def arg(i: Int): AbsValue = args.lift(i).getOrElse(AbsValue(LUndef))
      // the key variable of a property access, to detect index-preserving
      // copies (intent: `with-element`)
      def keyTag(i: Int): Option[String] =
        if (!intent.contains(Intent.WithElement)) None
        else
          argExprs.lift(i).collect {
            case ERef(x: Local) => s"$callerName:${localName(x)}"
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
          val v = arg(0).locs.foldLeft(Bot)(_ ⊔ heapRead(_, KField(iterElem)))
          if (v.isBottom) Clean else v
        case "GetMethod"                                  => Clean
        case "CreateBuiltinFunction"                      => arg(0)
        case "ThrowCompletion"                            => Bot
        case "ToString" | "ToPrimitive" | "ToPropertyKey" => toStr(arg(0))
        // remember the tested local to refine its branches
        // (intent: `without-element`)
        case "IsArray" if intent.contains(Intent.WithoutElement) =>
          val tested = argExprs.headOption.collect { case ERef(x: Local) => x }
          AbsValue(cst = Cst.Top, isArrayOf = tested.toSet)
        case _ if cleanOps.contains(fname) => Clean
        case "Get" | "GetV" =>
          readProp(arg(0), contentOf(arg(1)), keyTag(1))
        case "Set" | "CreateDataProperty" | "CreateDataPropertyOrThrow" =>
          writeProp(arg(0), contentOf(arg(1)), arg(2), keyTag(1)); Clean
        case "DefinePropertyOrThrow" | "DefineProperty" =>
          val desc = arg(2).locs.foldLeft(Bot)(_ ⊔ heapRead(_, KField("Value")))
          writeProp(arg(0), contentOf(arg(1)), desc); Clean
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
          callJS(arg(0), arg(1), args.lift(2).getOrElse(Bot), caller)
        case "Construct" => alloc(site)
        case "Invoke" =>
          val f = readProp(arg(0), contentOf(arg(1)))
          callJS(f, arg(0), args.lift(2).getOrElse(Bot), caller)
        case _ => return None,
      )

    def toStr(v: AbsValue): AbsValue = v.cst match
      case Cst.One(lit) if v.srcs.isEmpty && v.locs.isEmpty =>
        litToStr(lit).fold(Clean)(s => AbsValue(LStr(s)))
      case _ =>
        val contents = for { l <- v.locs; (_, cv) <- props(l) } yield cv
        val all = contents.foldLeft(v)(_ ⊔ _)
        AbsValue(srcs = all.srcs.map(_.toTaint), cst = Cst.Top)

    /** a call to a JS function value */
    def callJS(
      f: AbsValue,
      thisV: AbsValue,
      argList: AbsValue,
      caller: Caller,
    ): AbsValue =
      var res = Bot
      for (s <- f.srcs if s.value && s.path.depth < maxDepth)
        val fp = s.path
        flowsTo(thisV, fp.add("Parameter[this]"), fp.depth + 1)
        for (i <- 0 until maxParams)
          flowsTo(listElem(argList, i), fp.add(s"Parameter[$i]"), fp.depth + 1)
        res = res ⊔ src(fp.add("ReturnValue"))
      // built-in functions created from abstract closures
      for (clo <- f.clos)
        val args = List(thisV, argList, AbsValue(LUndef))
        doCall(Some(caller), clo.fname, args, clo.captured)
      if (res.isBottom && f.clos.isEmpty) Clean else res

    // =========================================================================
    // instructions
    // =========================================================================
    /** transfer function for normal instructions */
    def transfer(inst: NormalInst)(using np: NodePoint[?]): Updater =
      inst match
        case IExpr(EYet(msg)) => yetAssign(msg)
        case IExpr(expr) =>
          for { _ <- transfer(expr) } yield ()
        case ILet(x, expr) =>
          for {
            v <- transfer(expr)
            _ <- modify(_.define(x, v))
          } yield ()
        case IAssign(ref, expr) =>
          for {
            v <- transfer(expr)
            _ <- modify(assign(ref, v))
          } yield ()
        case IAssert(cond) =>
          modify(_.markComps(compChecked(cond)))
        case IExpand(_, _) | IDelete(_, _) | IPrint(_) | INop() => st => st
        case IPush(elem, list, _) =>
          for {
            v <- transfer(elem)
            lv <- transfer(list)
            _ <- modify(push(list, lv, v))
          } yield ()
        case IPop(lhs, list, front) =>
          for {
            lv <- transfer(list)
            _ <- modify(pop(lhs, list, lv, front))
          } yield ()
        case IReturn(expr) =>
          for {
            v <- transfer(expr)
            _ <- put(AbsState.Bot)
          } yield doReturn(v)

    /** assign a value to a reference */
    def assign(ref: Ref, v: AbsValue)(using np: NodePoint[Node]): Updater =
      ref match
        case x: Local  => _.define(x, v)
        case Global(_) => st => st
        case Field(base, kexpr) =>
          for {
            bv <- transfer(base)
            k <- transfer(kexpr)
          } yield for (l <- bv.locs) heapWrite(l, fieldKey(k), v)

    /** push a value to a list */
    def push(list: Expr, lv: AbsValue, v: AbsValue): Updater = st =>
      for (l <- lv.locs) heapWrite(l, KAny, v)
      (list, lv.rest.nonEmpty) match
        // materialize the remaining arguments as a list
        case (ERef(x: Local), true) =>
          val nl = alloc(nextSite())
          for (l <- nl.locs) heapWrite(l, KAny, listElems(lv) ⊔ v)
          val nv = lv.copy(
            rest = Set(),
            locs = lv.locs ++ nl.locs,
            popped = Unknown,
          )
          st.update(_ + (x -> nv))
        case _ => st

    /** pop a value from a list */
    def pop(lhs: Local, list: Expr, lv: AbsValue, front: Boolean): Updater =
      st =>
        val elem = if (front) listElem(lv, 0) else listElems(lv)
        val popped = st.update(_ + (lhs -> elem))
        // popping from the front of `ArgumentsList` shifts its offset
        list match
          case ERef(x: Local) if front =>
            val n =
              if (lv.popped < 0 || lv.popped == Empty) lv.popped
              else lv.popped + 1
            val shifted = lv.copy(rest = lv.rest.map(_ + 1), popped = n)
            popped.update(_ + (x -> shifted))
          case _ => popped

    // =========================================================================
    // refinement of branches
    // =========================================================================
    /** refine states for both branches of a condition */
    def refine(cond: Expr, st0: AbsState): (AbsState, AbsState) =
      val st = st0.markComps(compChecked(cond))
      val (thenSt, elseSt) = cond match
        // abrupt completions are not represented; the value is normal
        case _ if abruptVars(cond).nonEmpty =>
          (st.update(_ ++ abruptVars(cond).map(_ -> Bot)), st)
        case EBinary(BOp.Lt, EMath(n), ESizeOf(ERef(x: Local)))
            if n == 0 && st.locals.contains(x) =>
          // the list is empty in the else branch
          val v = st.locals(x)
          (st, st.update(_ + (x -> v.copy(rest = Set(), popped = Empty))))
        case _ => (st, st)
      isArrayTest(cond, st) match
        case Some((tested, positive)) =>
          def mark(st: AbsState): AbsState = st.update(_.map { (x, v) =>
            if (tested.contains(x)) x -> v.nonArray else x -> v
          })
          if (positive) (thenSt, mark(elseSt)) else (mark(thenSt), elseSt)
        case None => (thenSt, elseSt)

    /** the locals tested by `IsArray` in a condition, and whether the condition
      * holds when the value is an array
      */
    def isArrayTest(cond: Expr, st: AbsState): Option[(Set[Local], Boolean)] =
      def tested(x: Local) = st.get(x).map(_.isArrayOf).filter(_.nonEmpty)
      cond match
        case ERef(x: Local) => tested(x).map((_, true))
        case EBinary(BOp.Eq, ERef(x: Local), EBool(b)) => tested(x).map((_, b))
        case EUnary(UOp.Not, c) => isArrayTest(c, st).map((s, b) => (s, !b))
        case _                  => None

    // =========================================================================
    // `yet` steps: approximate with the variables they mention
    // =========================================================================
    def yetVars(msg: String): List[Name] =
      varPattern.findAllMatchIn(msg).map(m => Name(m.group(1))).toList

    def yetAssign(msg: String): Updater = st =>
      yets += msg
      val vars = yetVars(msg)
      // "... calls to _F_ ...": call the closure with elements of the lists
      for {
        m <- callsToPattern.findAllMatchIn(msg)
        f = Name(m.group(1))
        fv <- st.get(f).toList
        clo <- fv.clos
      } {
        val elems = vars
          .filter(_ != f)
          .flatMap(st.get)
          .foldLeft(Bot)((acc, v) => acc ⊔ listElems(v))
        doCall(None, clo.fname, List(elems, elems), clo.captured)
      }
      assignPattern.findFirstMatchIn(msg) match
        case Some(m) =>
          val x = Name(m.group(2))
          st.update(_ + (x -> yetValue(vars.filter(_ != x), st)))
        case None => st

    def yetValue(vars: List[Name], st: AbsState): AbsValue =
      vars.flatMap(st.get).foldLeft(Clean)((acc, v) => acc ⊔ toStr(v))

    // =========================================================================
    // expressions
    // =========================================================================
    /** transfer function for expressions */
    def transfer(expr: Expr)(using np: NodePoint[Node]): Result[AbsValue] =
      expr match
        case EStr(s)        => AbsValue(LStr(s))
        case ENumber(d)     => AbsValue(LNum(d))
        case EMath(n)       => AbsValue(LMath(n))
        case EInfinity(pos) => AbsValue(LInf(pos))
        case EBool(b)       => AbsValue(LBool(b))
        case EUndef()       => AbsValue(LUndef)
        case ENull()        => AbsValue(LNull)
        case EEnum(name)    => AbsValue(LEnum(name))
        case _: LiteralExpr => Clean
        case ERef(ref)      => transfer(ref)
        case unary @ EUnary(_, e) =>
          for {
            v <- transfer(e)
            st <- get
          } yield transfer(st, unary, v)
        case binary @ EBinary(_, l, r) =>
          for {
            lv <- transfer(l)
            rv <- transfer(r)
            st <- get
          } yield transfer(st, binary, lv, rv)
        case EVariadic(vop, es) =>
          for {
            vs <- join(es.map(transfer))
            st <- get
          } yield transfer(st, vop, vs)
        case EMathOp(mop, es) =>
          for {
            vs <- join(es.map(transfer))
            st <- get
          } yield transfer(st, mop, vs)
        case ESubstring(e, from, to) =>
          for {
            _ <- transfer(from)
            _ <- join(to.map(transfer))
            v <- transfer(e)
          } yield toStr(v)
        case ETrim(e, _) =>
          for { v <- transfer(e) } yield toStr(v)
        case EConvert(COp.ToStr(_) | COp.ToCodeUnit, e) =>
          for { v <- transfer(e) } yield toStr(v)
        case EConvert(cop, e) =>
          for { v <- transfer(e) } yield convert(cop, v)
        case EExists(x: Local) =>
          for { st <- get } yield st.get(x) match
            case None    => AbsValue(LBool(false))
            case Some(v) => bool(true, v.maybeAbsent)
        case EExists(_) => Clean
        case ETypeCheck(e, _) =>
          for { _ <- transfer(e) } yield Clean
        case EClo(fname, captured) =>
          for { st <- get } yield
            val cap = captured.map(x => x -> st.get(x).getOrElse(Bot))
            AbsValue(clos = Set(AbsClo(fname, cap.toMap)), cst = Cst.Top)
        case e: AllocExpr =>
          for {
            l <- get(_ => Loc(nextSite()))
            _ <- transfer(e, l)
          } yield AbsValue(locs = Set(l), cst = Cst.Top)
        case EParse(code, rule) =>
          for {
            _ <- transfer(rule)
            v <- transfer(code)
          } yield toStr(v)
        case EYet(msg) =>
          for { st <- get } yield
            yets += msg
            yetValue(yetVars(msg), st)
        case _ => Clean

    /** transfer function for allocation expressions */
    def transfer(e: AllocExpr, l: Loc)(using NodePoint[Node]): Result[Unit] =
      e match
        case ERecord(_, pairs) =>
          for {
            _ <- join(pairs.map { (f, fe) =>
              for { v <- transfer(fe) } yield heapWrite(l, KField(f), v)
            })
          } yield ()
        case EList(es) =>
          for {
            _ <- join(es.zipWithIndex.map { (fe, i) =>
              for { v <- transfer(fe) } yield heapWrite(l, KIdx(i), v)
            })
          } yield ()
        case ECopy(obj) =>
          for { ov <- transfer(obj) } yield
            for (ol <- ov.locs; (k, kv) <- heapObj(ol)) heapWrite(l, k, kv)
            if (ov.rest.nonEmpty) heapWrite(l, KAny, listElems(ov))
        case EMap(_, pairs) =>
          for {
            _ <- join(pairs.map { (ke, ve) =>
              for {
                _ <- transfer(ke)
                v <- transfer(ve)
              } yield heapWrite(l, KAny, v)
            })
          } yield ()
        case EKeys(_, _) =>
          for { _ <- get } yield heapWrite(l, KAny, AbsValue(LOpaque))

    /** transfer function for references */
    def transfer(ref: Ref)(using np: NodePoint[Node]): Result[AbsValue] =
      ref match
        case x: Local => get(_.get(x).getOrElse(Bot).present)
        case Global(g) if g.startsWith("SYMBOL.") =>
          AbsValue(LSym(g.stripPrefix("SYMBOL.")))
        case Global(_) => Clean
        case Field(base, kexpr) =>
          for {
            bv <- transfer(base)
            k <- transfer(kexpr)
            st <- get
          } yield field(base, bv, k, st)

    /** read a field of a value */
    def field(base: Ref, bv: AbsValue, k: AbsValue, st: AbsState): AbsValue =
      def isComp = base match
        case x: Local => st.isComp(x)
        case _        => false
      k.cst match
        // completion records are not represented, but other records may
        // have a `Value` field
        case Cst.One(LStr("Value")) if isComp => bv
        case Cst.One(LStr("Value")) =>
          bv.locs.foldLeft(bv.copy(locs = Set())) { (acc, l) =>
            heapObj(l).get(KField("Value")) match
              case Some(v) => acc ⊔ v
              case None    => acc ⊔ AbsValue(locs = Set(l), cst = Cst.Top)
          }
        case _ =>
          var res = Bot
          if (bv.rest.nonEmpty) res = res ⊔ (k.cst match
            case Cst.One(lit) =>
              lit.toDouble.fold(Bot)(d => listElem(bv, d.toInt))
            case _ => listElems(bv)
          )
          for (s <- bv.srcs)
            res = res ⊔ AbsValue(srcs = Set(s.toTaint), cst = Cst.Top)
          for (l <- bv.locs)
            val obj = heapObj(l)
            fieldKey(k) match
              case KAny =>
                for ((k, v) <- obj if k == KAny || k.isInstanceOf[KIdx])
                  res = res ⊔ v
              case kk @ KIdx(_) =>
                res = res ⊔ obj.getOrElse(kk, Bot) ⊔ obj.getOrElse(KAny, Bot)
              case kk => res = res ⊔ obj.getOrElse(kk, Bot)
          if (res.isBottom) Clean else res

    def fieldKey(k: AbsValue): Key = k.cst match
      case Cst.One(LStr(s)) => KField(s)
      case Cst.One(lit)     => lit.toDouble.fold(KAny)(d => KIdx(d.toInt))
      case _                => KAny

    /** transfer function for unary operators */
    def transfer(st: AbsState, unary: EUnary, operand: AbsValue)(using
      np: NodePoint[Node],
    ): AbsValue = unary.uop match
      case UOp.Not => bool(operand.canBeFalse, operand.canBeTrue)
      case UOp.Neg => numOp(operand, operand, (a, _) => -a)
      case _       => Clean

    /** transfer function for binary operators */
    def transfer(
      st: AbsState,
      binary: EBinary,
      left: AbsValue,
      right: AbsValue,
    )(using np: NodePoint[Node]): AbsValue = (binary.bop, left, right) match
      case (BOp.Eq | BOp.Equal, l, r) =>
        (l.cst, r.cst) match
          case (Cst.One(a), Cst.One(b)) if l.srcs.isEmpty && r.srcs.isEmpty =>
            val eq = (a.toDouble, b.toDouble) match
              case (Some(x), Some(y)) => x == y
              case _                  => a == b
            AbsValue(LBool(eq))
          case _ => Clean
      case (BOp.Lt, l, r) =>
        (l.cst, r.cst) match
          case (Cst.One(a), Cst.One(b)) =>
            (a.toDouble, b.toDouble) match
              case (Some(x), Some(y)) => AbsValue(LBool(x < y))
              case _                  => Clean
          case _ => Clean
      case (BOp.And, l, r) =>
        bool(l.canBeTrue && r.canBeTrue, l.canBeFalse || r.canBeFalse)
      case (BOp.Or, l, r) =>
        bool(l.canBeTrue || r.canBeTrue, l.canBeFalse && r.canBeFalse)
      case (BOp.Add, l, r) => numOp(l, r, _ + _)
      case (BOp.Sub, l, r) => numOp(l, r, _ - _)
      case (BOp.Mul, l, r) => numOp(l, r, _ * _)
      case _               => Clean

    /** transfer for variadic operators */
    def transfer(st: AbsState, vop: VOp, vs: List[AbsValue])(using
      np: NodePoint[Node],
    ): AbsValue = vop match
      case VOp.Concat =>
        val strs = vs.map(_.cst).collect { case Cst.One(LStr(s)) => s }
        if (strs.length == vs.length) AbsValue(LStr(strs.mkString))
        else vs.foldLeft(Clean)((acc, v) => acc ⊔ toStr(v))
      case _ => Clean

    /** transfer for mathematical operators */
    def transfer(st: AbsState, mop: MOp, vs: List[AbsValue])(using
      np: NodePoint[Node],
    ): AbsValue = Clean

    /** conversion of constants */
    def convert(cop: COp, v: AbsValue): AbsValue = v.cst match
      case Cst.One(lit) if v.srcs.isEmpty =>
        (cop, lit.toDouble) match
          case (COp.ToNumber | COp.ToApproxNumber, Some(d)) => AbsValue(LNum(d))
          case (COp.ToMath, Some(d)) if !d.isInfinite =>
            AbsValue(LMath(BigDecimal(d)))
          case _ => Clean
      case _ => Clean

    def numOp(
      l: AbsValue,
      r: AbsValue,
      f: (Double, Double) => Double,
    ): AbsValue = (l.cst, r.cst) match
      case (Cst.One(a), Cst.One(b)) =>
        (a, b, a.toDouble, b.toDouble) match
          case (_: LNum, _: LNum, Some(x), Some(y)) => AbsValue(LNum(f(x, y)))
          case (_, _, Some(x), Some(y)) =>
            val z = f(x, y)
            if (z.isInfinite) AbsValue(LInf(z > 0))
            else AbsValue(LMath(BigDecimal(z)))
          case _ => Clean
      case _ => Clean
  }
}
