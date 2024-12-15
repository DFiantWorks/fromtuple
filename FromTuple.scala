package fromtuple
import macros.*
import collection.immutable.ListMap

type ~[T] = FromTuple[T]
opaque type FromTuple[T] <: T = T
object FromTuple:
  inline implicit def fromTuple[F, T](inline from: F): FromTuple[T] =
    ${ fromTupleMacro[F, T]('from) }

inline implicit def conversion[F, T, O >: Id[T] <: Id[T]](using AllowedConversion[F, T])(
    inline from: F
): O = ${ tupleToXMacro[F, O]('from) }
