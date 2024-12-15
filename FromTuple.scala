package fromtuple
import macros.*

type ~[T] = FromTuple[T]
opaque type FromTuple[T] <: T = T
object FromTuple:
  protected class Wrap[T]
  def apply[T]: Wrap[T] = ???
  inline implicit def fromTuple[F <: Tuple, T](inline from: F): FromTuple[T] =
    ${ fromTupleMacro[F, T]('from) }
  inline implicit def fromT[T](inline from: T): FromTuple[T] = from
