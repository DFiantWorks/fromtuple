package fromtuple
import scala.quoted.*
import scala.collection.immutable.{ListMap, ListSet}
private object macros:
  // helper alias to prevent widening in implicit conversion return type
  type Id[T] = T
  // implicit limiter to when a conversion should trigger
  trait AllowedConversion[F, T]
  trait AllowedConversionLP:
    object OK extends AllowedConversion[Any, Any]
    inline given fromElement[F, T <: Iterable[F]]: AllowedConversion[F, T] =
      OK.asInstanceOf[AllowedConversion[F, T]]
  object AllowedConversion extends AllowedConversionLP:
    inline given fromTuple[F <: Tuple, T]: AllowedConversion[F, T] =
      OK.asInstanceOf[AllowedConversion[F, T]]

  extension [Q <: Quotes](using q: Q)(tpe: q.reflect.TypeRepr)
    def asTypeAny: Type[Any] =
      import quotes.reflect.*
      tpe.asType.asInstanceOf[Type[Any]]
    def asTypeTree: quotes.reflect.TypeTree =
      import quotes.reflect.*
      tpe.asType match
        case '[t] =>
          TypeTree.of[t]
  extension [Q <: Quotes](using q: Q)(term: q.reflect.Term)
    def getTupleArgs: Option[List[q.reflect.Term]] =
      import quotes.reflect.*
      term.underlying match
        // for inline tuples up to 23 elements
        case Apply(TypeApply(Select(Ident(tplClsName), "apply"), _), args)
            if (tplClsName.startsWith("Tuple")) =>
          Some(args)
        case TypeApply(
              Select(
                Apply(TypeApply(Select(Ident(tplClsName), "apply"), _), args),
                _
              ),
              _
            ) if (tplClsName.startsWith("Tuple")) =>
          Some(args)
        // for inline tuples larger than 23 elements
        case TypeApply(
              Select(
                Apply(
                  Select(Ident("TupleXXL"), "apply"),
                  Typed(Repeated(args, _), _) :: _
                ),
                _
              ),
              _
            ) =>
          Some(args)
        case x =>
          println(x.show)
          None
  def convertRecur[Q <: Quotes](using
      q: Q
  )(
      from: q.reflect.Term,
      toTpe: q.reflect.TypeRepr
  ): Option[q.reflect.Term] =
    convertRecurPos(from, toTpe, from.pos)
  def convertRecurPos[Q <: Quotes](using
      q: Q
  )(
      from: q.reflect.Term,
      toTpe: q.reflect.TypeRepr,
      fromPos: q.reflect.Position
  ): Option[q.reflect.Term] =
    import quotes.reflect.*
    val fromType = from.tpe.asTypeAny
    def conversionSummon: Option[Term] =
      val toType = toTpe.asTypeAny
      val convertionTpe =
        AppliedType(Symbol.requiredClass("scala.Conversion").typeRef, List(from.tpe, toTpe))
      Implicits.search(convertionTpe) match
        case succ: ImplicitSearchSuccess =>
          val conv = succ.tree.asExprOf[Conversion[fromType.Underlying, toType.Underlying]]
          Some('{ ${ conv }(${ from.asExprOf[fromType.Underlying] }) }.asTerm)
        case _: NoMatchingImplicits =>
          report.error(
            s"Found: (${from.show} : ${from.tpe.widen.show})\nRequired: ${toTpe.show}",
            fromPos
          )
          None
        case fail: ImplicitSearchFailure =>
          report.error(fail.explanation, fromPos)
          None
    // currently a naive implementation that does not ignore whitespaces
    // lazy val lhs = Position(fromPos.sourceFile, fromPos.start - 1, fromPos.start).sourceCode
    // lazy val rhs = Position(fromPos.sourceFile, fromPos.end, fromPos.end + 1).sourceCode
    // val singleElementTuple = false
    // (lhs, rhs) match
    //   case (Some("("), Some(")")) => true
    //   case _                      => false
    // if (singleElementTuple)
    //   val updatedFrom = '{
    //     Tuple1[fromType.Underlying](${ from.asExprOf[fromType.Underlying] })
    //   }.asTerm
    //   val updatedPos = Position(fromPos.sourceFile, fromPos.start - 1, fromPos.end + 1)
    //   convertRecurPos(updatedFrom, toTpe, updatedPos)
    // if the from term type matches the target type, then return it as is
    if (from.tpe <:< toTpe) Some(from)
    else
      from.tpe.asType match
        // Int weak conformance
        case '[Int] =>
          toTpe.asType match
            // Int to Long weak conformance
            case '[Long] => Some('{ ${ from.asExprOf[Int] }.toLong }.asTerm)
            // Int to Float weak conformance
            case '[Float] => Some('{ ${ from.asExprOf[Int] }.toFloat }.asTerm)
            // Int to Double weak conformance
            case '[Double] => Some('{ ${ from.asExprOf[Int] }.toDouble }.asTerm)
            case _         => conversionSummon
        // Dedicated Tuple conversion
        case '[Tuple] =>
          from.getTupleArgs match
            case Some(tupleArgs) =>
              def varArgsExpr[A: Type, C[_]: Type](
                  constr: List[Expr[A]] => Expr[C[A]]
              ): Option[Term] =
                val listArgs =
                  tupleArgs.map(a => convertRecur(a, TypeRepr.of[A]).map(_.asExprOf[A]))
                if (listArgs.exists(_.isEmpty)) None
                else Some(constr(listArgs.flatten).asTerm)
              def varArgsPairExpr[K: Type, V: Type, C[_, _]: Type](
                  constr: List[Expr[(K, V)]] => Expr[C[K, V]]
              ): Option[Term] =
                val mapArgs = tupleArgs.map(a =>
                  a.asExpr match
                    case '{ { ${ key }: Any } -> $value } =>
                      // converting and reconstructing key and value pairs
                      (
                        convertRecur(key.asTerm, TypeRepr.of[K]),
                        convertRecur(value.asTerm, TypeRepr.of[V])
                      ) match
                        case (Some(key), Some(value)) =>
                          Some('{ ${ key.asExprOf[K] } -> ${ value.asExprOf[V] } })
                        case _ =>
                          None
                    case _ =>
                      report.error("Invalid `key -> value` pattern for Map", a.pos)
                      None
                )
                if (mapArgs.exists(_.isEmpty)) None
                else Some(constr(mapArgs.flatten).asTerm)
              toTpe.asType match
                // Tuple to Map/ListMap conversion
                case '[ListMap[k, v]] =>
                  varArgsPairExpr[k, v, ListMap](x => '{ ListMap[k, v](${ Varargs(x) }*) })
                case '[Map[k, v]] =>
                  varArgsPairExpr[k, v, Map](x => '{ Map[k, v](${ Varargs(x) }*) })
                // Tuple to List/ListSet/Seq/Set conversion
                case '[ListSet[t]] =>
                  varArgsExpr[t, ListSet](x => '{ ListSet[t](${ Varargs(x) }*) })
                case '[List[t]] => varArgsExpr[t, List](x => '{ List[t](${ Varargs(x) }*) })
                case '[Seq[t]]  => varArgsExpr[t, Seq](x => '{ Seq[t](${ Varargs(x) }*) })
                case '[Set[t]]  => varArgsExpr[t, Set](x => '{ Set[t](${ Varargs(x) }*) })
                // Tuple to new class instance conversion
                case _ =>
                  val toSym = toTpe.typeSymbol
                  val constr = toSym.primaryConstructor
                  // only classes with a single parameter block are supported
                  val (paramSyms, typeParamSyms) = constr.paramSymss match
                    // has type parameters
                    case typeParamSyms :: paramSyms :: Nil if toTpe.typeArgs.nonEmpty =>
                      (paramSyms, typeParamSyms)
                    // has no type parameters
                    case paramSyms :: Nil if toTpe.typeArgs.isEmpty =>
                      (paramSyms, Nil)
                    case _ => (Nil, Nil)
                  if (toSym.isClassDef && paramSyms.nonEmpty)
                    if (paramSyms.length != tupleArgs.length)
                      report.error(
                        s"Expected number of arguments for `$toSym` is ${paramSyms.length}, but found ${tupleArgs.length}",
                        fromPos
                      )
                      None
                    else
                      val applyArgs = paramSyms
                        .lazyZip(tupleArgs)
                        .map((f, a) =>
                          convertRecur(
                            a,
                            f.termRef.widen.substituteTypes(typeParamSyms, toTpe.typeArgs)
                          )
                        )
                      if (applyArgs.exists(_.isEmpty)) None
                      // new class instance without type argument
                      else if (toTpe.typeArgs.isEmpty)
                        Some(
                          Apply(
                            Select(New(TypeIdent(toSym)), constr),
                            applyArgs.flatten
                          )
                        )
                      // new class instance with type arguments
                      else
                        Some(
                          Apply(
                            TypeApply(
                              Select(New(TypeIdent(toSym)), constr),
                              toTpe.typeArgs.map(_.asTypeTree)
                            ),
                            applyArgs.flatten
                          )
                        )
                  else conversionSummon
                case _ => conversionSummon
            case None => conversionSummon
        // Basic conversion using scala.Conversion
        case _ => conversionSummon

  def fromTupleMacro[F: Type, T: Type](using Quotes)(
      from: Expr[F]
  ): Expr[FromTuple[T]] =
    import quotes.reflect.*
    convertRecur(from.asTerm, TypeRepr.of[T])
      .map(toTerm => '{ ${ toTerm.asExpr }.asInstanceOf[FromTuple[T]] })
      .getOrElse(report.errorAndAbort("Tuple conversion error"))

  def tupleToXMacro[F: Type, X: Type](using Quotes)(
      from: Expr[F]
  ): Expr[X] =
    import quotes.reflect.*
    convertRecur(from.asTerm, TypeRepr.of[X].dealias)
      .map(toTerm => '{ ${ toTerm.asExpr }.asInstanceOf[X] })
      .getOrElse(report.errorAndAbort("Tuple conversion error"))
