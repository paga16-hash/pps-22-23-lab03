package lab03

import org.junit.*
import org.junit.Assert.*
import lab03.Lists.*
import lab03.Lists.List.flatMap
import u02.Modules.Person

class ListTest:

  import List.*

  val l: List[Int] = Cons(10, Cons(20, Cons(30, Nil())))

  @Test def testSum(): Unit =
    assertEquals(0, sum(Nil()))
    assertEquals(60, sum(l))

  @Test def testMap(): Unit =
    assertEquals(Cons(11, Cons(21, Cons(31, Nil()))), map(l)(_ + 1))
    assertEquals(Cons("10", Cons("20", Cons("30", Nil()))), map(l)(_ + ""))

  @Test def testFilter(): Unit =
    assertEquals(Cons(20, Cons(30, Nil())), filter(l)(_ >= 20))
    assertEquals(Cons(10, Cons(30, Nil())), filter(l)(_ != 20))

  @Test def testDrop(): Unit =
    assertEquals(Cons(20, Cons(30, Nil())), drop(l, 1))
    assertEquals(Cons(30, Nil()), drop(l, 2))
    assertEquals(Nil(), drop(l, 5))

  @Test def testAppend(): Unit =
    val tail = Cons(40, Nil())
    assertEquals(Cons(10, Cons(20, Cons(30, Cons(40, Nil())))), append(l, tail))
    assertEquals(l, append(l, Nil()))

  @Test def testFlatMap(): Unit =
    assertEquals(Cons(11, Cons(21, Cons(31, Nil()))), flatMap(l)(v => Cons(v + 1, Nil())))
    assertEquals(Cons(11, Cons(12, Cons(21, Cons(22, Cons(31, Cons(32, Nil())))))), flatMap(l)(v => Cons(v + 1, Cons(v + 2, Nil()))))

  @Test def testMax(): Unit =
    import u02.Optionals.Option.*
    assertEquals(None(), max(Nil()))
    assertEquals(Some(25), max(Cons(10, Cons(25, Cons(20, Nil())))))

  @Test
  def testGetCourses(): Unit =
    import u02.Optionals.Option.*
    import u02.Modules.Person.*
    val tl: List[Person] = Cons(Person.Teacher("p1", "c1"), Cons(Person.Teacher("p2", "c2"), Cons(Person.Teacher("p3", "c3"), Nil())))
    assertEquals(Cons("c1", Cons("c2", Cons("c3", Nil()))), getCourses(tl))
    val pl: List[Person] = Cons(Person.Student("p1", 2023), Cons(Person.Teacher("p2", "c2"), Cons(Person.Student("p3", 2023), Nil())))
    assertEquals(Cons("c2", Nil()), getCourses(pl))
    assertEquals(Nil(), getCourses(Nil()))

  @Test
  def testFoldLeft(): Unit =
    val lt = Cons(3, Cons(7, Cons(1, Cons(5, Nil()))))
    assertEquals(-16, foldLeft(lt)(0)(_ - _))
    assertEquals(16, foldLeft(lt)(0)(_ + _))
    assertEquals(105, foldLeft(lt)(1)(_ * _))

  @Test
  def testFoldRight(): Unit =
    //val lt = Cons(5, Cons(1, Cons(7, Cons(3, Nil()))))
    //assertEquals(-8, foldLeft(lt)(0)(_ - _))
    val lt = Cons(3, Cons(7, Cons(1, Cons(5, Nil()))))
    assertEquals(-8, foldRight(lt)(0)(_ - _))
    assertEquals(16, foldRight(lt)(0)(_ + _))

  import lab03.Lists.Stream.*

  @Test
  def testStreamDrop(): Unit =
    val l = Cons(6, Cons(7, Cons(8, Cons(9, Nil()))))
    val s = Stream.take(Stream.iterate(0)(_ + 1))(10)
    assertEquals(l, Stream.toList(Stream.drop(s)(6)))

  @Test
  def testStreamConstant(): Unit =
    val l = Cons("x", Cons("x", Cons("x", Cons("x", Cons("x", Nil())))))
    assertEquals(l, Stream.toList(Stream.take(constant("x"))(5)))

  @Test
  def testStreamFibs(): Unit =
    val l = Cons(0, Cons(1, Cons(1, Cons(2, Cons(3, Cons(5, Cons(8, Cons(13, Nil()))))))))
    assertEquals(l, Stream.toList(Stream.take(fibst)(8)))
