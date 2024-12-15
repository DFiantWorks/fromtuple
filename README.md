# fromtuple
Scala 3 Tuple to Collections and Classes Conversion Library

## Examples
```scala
import fromtuple.conversion
import collection.immutable.{ListMap, ListSet}
val l1:  List[Int]          = (1, 2)
val ll1: List[List[Int]]    = (l1, l1)
val ll2: List[List[Int]]    = ((1, 2), (3, 4))
val ll3: List[List[Long]]   = ((1, 2), (3, 4))
val ll4: List[List[Double]] = ((1, 2), (3, 4))
val l2:  Seq[Int]           = (1, 2)
val ll5: Set[Seq[Int]]      = (l2, l2)
val ll6: Seq[ListSet[Int]]  = ((1, 2), (3, 4))
val m1:  Map[String, Int]               = ("k1" -> 1, "k2" -> 2)
val m2:  ListMap[Int, String]           = (1 -> "v1", 2 -> "v2")
val ml1: Map[String, List[Int]]         = ("k1" -> (1, 2), "k2" -> (3, 4), "k3" -> l1)
val ml2: ListMap[Double, ListSet[Long]] = (1 -> (1, 2), 2.0 -> (3L, 4), 3 -> (1, 2L))
case class Foo[T](x: T, y: Int)
class Bar(val x: Int, val y: Int, val z: String)
val c1:  Foo[Int] = (1, 2)
val c2:  Foo[String] = ("1", 2)
val c3:  Bar = (1, 2, "3")
val c4:  Foo[List[Int]] = ((1, 2, 3), 4)
```
