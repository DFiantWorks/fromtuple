import fromtuple.conversion
import munit.*
import compiletime.testing.{typeCheckErrors, Error}
import collection.immutable.{ListMap, ListSet}

class ConversionSpec extends FunSuite:
  transparent inline def assertCompiletimeError(
      inline code: String,
      expectedErrors: (Int, String)*
  ): Unit =
    val errors = typeCheckErrors(code)
    assertEquals(errors.map(e => (e.column, e.message)).toSeq, expectedErrors)

  test("Tuple to List"):
    val l1: List[Int] = (1, 2)
    assertEquals(l1, List(1, 2))
    val ll1: List[List[Int]] = (l1, l1)
    assertEquals(ll1, List(List(1, 2), List(1, 2)))
    val ll2: List[List[Int]] = ((1, 2), (3, 4))
    assertEquals(ll2, List(List(1, 2), List(3, 4)))
    val ll3: List[List[Long]] = ((1, 2), (3, 4))
    assertEquals(ll3, List(List(1L, 2L), List(3L, 4L)))
    val ll4: List[List[Double]] = ((1, 2), (3, 4))
    assertEquals(ll4, List(List(1.0, 2.0), List(3.0, 4.0)))
    assertCompiletimeError(
      """
      val l1: List[Int] = (1, "2")
      """,
      26 -> """Tuple conversion error""",
      30 -> s"""Found: ("2" : java.lang.String)\nRequired: scala.Int"""
    )
    assertCompiletimeError(
      """
      val x: List[List[Int]] = ((1, 2.0), ("3", 4))
      """,
      26 -> """Tuple conversion error""",
      43 -> s"""Found: ("3" : java.lang.String)\nRequired: scala.Int""",
      36 -> s"""Found: (2.0 : scala.Double)\nRequired: scala.Int"""
    )

  test("Tuple to Seq/Set/ListSet"):
    val l1: Seq[Int] = (1, 2)
    assertEquals(l1, Seq(1, 2))
    val ll1: Set[Seq[Int]] = (l1, l1)
    assertEquals(ll1, Set(Seq(1, 2), Seq(1, 2)))
    val ll2: Seq[ListSet[Int]] = ((1, 2), (3, 4))
    assertEquals(ll2, Seq(ListSet(1, 2), ListSet(3, 4)))

  test("Conversion summoning"):
    given Conversion[String, Int] with
      def apply(x: String): Int = x.toIntOption.get
    val l1: List[Int] = (1, "2")
    assertEquals(l1, List(1, 2))
    transparent inline given Conversion[Int, String] = compiletime.error("My custom error")
    assertCompiletimeError(
      """
      val l2: List[String] = (1, "2")
      """,
      26 -> """Tuple conversion error""",
      30 -> """My custom error"""
    )

  test("Tuple to Map/ListMap"):
    val l1: List[Int] = (1, 2)
    val m1: Map[String, Int] = ("k1" -> 1, "k2" -> 2)
    assertEquals(m1, Map("k1" -> 1, "k2" -> 2))
    val m2: ListMap[Int, String] = (1 -> "v1", 2 -> "v2")
    assertEquals(m2, ListMap(1 -> "v1", 2 -> "v2"))
    val ml1: Map[String, List[Int]] = ("k1" -> (1, 2), "k2" -> (3, 4), "k3" -> l1)
    assertEquals(ml1, Map("k1" -> List(1, 2), "k2" -> List(3, 4), "k3" -> List(1, 2)))
    val ml2: ListMap[Double, ListSet[Long]] = (1 -> (1, 2), 2.0 -> (3L, 4), 3 -> (1, 2L))
    assertEquals(
      ml2,
      ListMap(1.0 -> ListSet(1L, 2L), 2.0 -> ListSet(3L, 4L), 3.0 -> ListSet(1L, 2L))
    )
    assertCompiletimeError(
      """
      val x: ListMap[Double, ListSet[Long]] = (1 -> (1, "2"), 2.0 -> (3L, 4), "3" -> (1, 2L), 4)
      """,
      26 -> """Tuple conversion error""",
      94 -> """Invalid `key -> value` pattern for Map""",
      78 -> s"""Found: ("3" : java.lang.String)\nRequired: scala.Double""",
      56 -> s"""Found: ("2" : java.lang.String)\nRequired: scala.Long"""
    )

  test("Tuple to new class instance"):
    case class Foo[T](x: T, y: Int)
    class Bar(val x: Int, val y: Int, val z: String)

    val c1: Foo[Int] = (1, 2)
    assertEquals(c1, Foo(1, 2))
    val c2: Foo[String] = ("1", 2)
    assertEquals(c2, Foo("1", 2))
    val c3: Bar = (1, 2, "3")
    assertEquals(c3.x, 1)
    assertEquals(c3.y, 2)
    assertEquals(c3.z, "3")
    val c4: Foo[List[Int]] = ((1, 2, 3), 4)
    assertEquals(c4, Foo(List(1, 2, 3), 4))
    assertCompiletimeError(
      """
      val x: Foo[Int] = ("1", 2)
      """,
      26 -> """Tuple conversion error""",
      25 -> s"""Found: ("1" : java.lang.String)\nRequired: scala.Int"""
    )
    assertCompiletimeError(
      """
      val x: Foo[Int] = (1, 2, 3)
      """,
      26 -> """Tuple conversion error""",
      25 -> """Expected number of arguments for `class Foo` is 2, but found 3"""
    )
