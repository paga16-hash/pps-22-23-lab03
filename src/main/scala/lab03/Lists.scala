package lab03

import u02.Modules.Person
import u02.Modules.Person.Teacher

import scala.:+
import scala.annotation.tailrec
import u02.Optionals.Option.*

object Lists extends App :

  // A generic linkedlist
  enum List[E]:
    case Cons(head: E, tail: List[E])
    case Nil()

  // a companion object (i.e., module) for List
  object List:

    def sum(l: List[Int]): Int = l match
      case Cons(h, t) => h + sum(t)
      case _ => 0

    def map[A, B](l: List[A])(mapper: A => B): List[B] = l match
      case Cons(h, t) => Cons(mapper(h), map(t)(mapper))
      case Nil() => Nil()

    def filter[A](l1: List[A])(pred: A => Boolean): List[A] = l1 match
      case Cons(h, t) if pred(h) => Cons(h, filter(t)(pred))
      case Cons(_, t) => filter(t)(pred)
      case Nil() => Nil()

    @tailrec
    def drop[A](l1: List[A], n: Int): List[A] = l1 match
      case Cons(h, t) if n == 0 => Cons(h, t)
      case Cons(_, t) => drop(t, n - 1)
      case Nil() => Nil()

    def append[A](l: List[A], r: List[A]): List[A] = (l, r) match
      case (Nil(), r) => r
      case (Cons(h, t), r) => Cons(h, append(t, r))

    def flatMap[A, B](l: List[A])(f: A => List[B]): List[B] = l match
      case Nil() => Nil()
      case Cons(h, Nil()) => f(h)
      case Cons(h, t) => append(f(h), flatMap(t)(f))
    //case Cons(h, t) => Cons(f(h).productElement(0).asInstanceOf[B], t); flatMap2(t)(f)

    import u02.Optionals.*

    def max(l: List[Int]): Option[Int] =
      @tailrec
      def intMax(l: List[Int], acc: Option[Int]): Option[Int] = (l, acc) match
        case (Cons(h, t), None()) => intMax(t, Some(h))
        case (Cons(h, t), acc) if h > orElse(acc, h) => intMax(t, Some(h))
        case (Cons(_, Nil()), acc) => acc
        case (Cons(_, t), acc) => intMax(t, acc)

      intMax(l, None())

    import u02.Modules.Person.*

    def getCourses(l: List[Person]): List[String] =
      flatMap(l) {
        case t: Person.Teacher => Cons(t.course, Nil())
        case _ => Nil()
      }

    @tailrec
    def foldLeft(l: List[Int])(acc: Int)(bin: (Int, Int) => Int): Int = l match
      case Cons(h, t) => foldLeft(t)(bin(acc, h))(bin)
      case Nil() => acc


    // 3 + (7 + (1 + (5 + 0)))
    /*@tailrec
    def foldRight(l: List[Int])(acc: Int)(bin: (Int, Int) => Int): Int = l match
      case Cons(h, t) => foldRight(t)(bin(acc, h) * -1)(bin);
      case Nil() => acc * -1*/

    @tailrec
    def foldRight(l: List[Int])(acc: Int)(bin: (Int, Int) => Int): Int = l match
      case Cons(h, t) => foldRight(t)(bin(acc, h) * -1)(bin);
      case Nil() => acc * -1

  enum Stream[A]:
    private case Empty()
    private case Cons(head: () => A, tail: () => Stream[A])

  object Stream:

    def empty[A](): Stream[A] = Empty()

    def cons[A](hd: => A, tl: => Stream[A]): Stream[A] =
      lazy val head = hd
      lazy val tail = tl
      Cons(() => head, () => tail)

    def toList[A](stream: Stream[A]): List[A] = stream match
      case Cons(h, t) => List.Cons(h(), toList(t()))
      case _ => List.Nil()

    def map[A, B](stream: Stream[A])(f: A => B): Stream[B] = stream match
      case Cons(head, tail) => cons(f(head()), map(tail())(f))
      case _ => Empty()

    def filter[A](stream: Stream[A])(pred: A => Boolean): Stream[A] = stream match
      case Cons(head, tail) if (pred(head())) => cons(head(), filter(tail())(pred))
      case Cons(head, tail) => filter(tail())(pred)
      case _ => Empty()

    def take[A](stream: Stream[A])(n: Int): Stream[A] = (stream, n) match
      case (Cons(head, tail), n) if n > 0 => cons(head(), take(tail())(n - 1))
      case _ => Empty()

    def iterate[A](init: => A)(next: A => A): Stream[A] =
      cons(init, iterate(next(init))(next))

    @tailrec
    def drop[A](stream: Stream[A])(n: Int): Stream[A] = (stream, n) match
      case (Cons(_, t), n) if n != 0 => drop(t())(n - 1)
      case (stream, 0) => stream
      case _ => Empty()

    def constant[A](c: A): Stream[A] = iterate(c)(c => c)


  end Stream

  val l = List.Cons(10, List.Cons(20, List.Cons(30, List.Nil())))
  println(List.sum(l)) // 60

  import List.*

  println(sum(map(filter(l)(_ >= 20))(_ + 1))) // 21+31 = 52
