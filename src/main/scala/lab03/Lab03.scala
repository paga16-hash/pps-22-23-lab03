package lab03

import u02.Modules.Person
import u02.Modules.Person.Teacher

import scala.:+
import scala.annotation.tailrec
import u02.Optionals.Option.*

object Lab03 extends App :

  enum List[E]:
    case Cons(head: E, tail: List[E])
    case Nil()

  object List:

    def sum(l: List[Int]): Int = l match
      case Cons(h, t) => h + sum(t)
      case _ => 0

    def map[A, B](l: List[A])(mapper: A => B): List[B] = l match
      case Cons(h, t) => Cons(mapper(h), map(t)(mapper))
      case Nil() => Nil()

    def filter[A](l: List[A])(pred: A => Boolean): List[A] = l match
      case Cons(h, t) if pred(h) => Cons(h, filter(t)(pred))
      case Cons(_, t) => filter(t)(pred)
      case Nil() => Nil()

    //Task 1a
    @tailrec
    def drop[A](l: List[A], n: Int): List[A] = l match
      case Cons(h, t) => if n == 0 then Cons(h, t) else drop(t, n - 1)
      case Nil() => Nil()

    //Task 1b
    def append[A](l: List[A], r: List[A]): List[A] = l match
      case Nil() => r
      case Cons(h, t) => Cons(h, append(t, r))

    //Task 1c
    def flatMap[A, B](l: List[A])(f: A => List[B]): List[B] = l match
      case Nil() => Nil()
      case Cons(h, t) => append(f(h), flatMap(t)(f))

    //Task 1d
    def mapFM[A, B](l: List[A])(mapper: A => B): List[B] =
      flatMap(l)(h => Cons(mapper(h), Nil()))

    //Task 1d
    def filterFM[A](l: List[A])(pred: A => Boolean): List[A] =
      flatMap(l)(h => h match
        case h if pred(h) => Cons(h, Nil())
        case _ => Nil()
      )

    import u02.Optionals.*

    //Task 2
    def max(l: List[Int]): Option[Int] =
      @tailrec
      def max(l: List[Int], acc: Option[Int]): Option[Int] = (l, acc) match
        case (Cons(h, t), None()) => max(t, Some(h))
        case (Cons(h, t), acc) if h > orElse(acc, h) => max(t, Some(h))
        case (Cons(_, Nil()), _) | (Nil(), _) => acc
        case (Cons(_, t), acc) => max(t, acc)

      max(l, None())

    import u02.Modules.Person.*

    //Task 3 with FM
    def getCourses(l: List[Person]): List[String] =
      flatMap(l) {
        case t: Person.Teacher => Cons(t.course, Nil())
        case _ => Nil()
      }

    //Task 4a
    @tailrec
    def foldLeft[A](l: List[A])(acc: A)(bin: (A, A) => A): A = l match
      case Cons(h, t) => foldLeft(t)(bin(acc, h))(bin)
      case Nil() => acc

    //Task 4b, without using foldLeft
    def foldRight[A](l: List[A])(acc: A)(bin: (A, A) => A): A = l match
      case Cons(h, t) => bin(h, foldRight(t)(acc)(bin))
      case Nil() => acc

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

    def take[A](stream: Stream[A])(n: Int): Stream[A] = stream match
      case Cons(h, t) if n > 0 => cons(h(), take(t())(n - 1))
      case _ => Empty()

    def iterate[A](init: => A)(next: A => A): Stream[A] =
      cons(init, iterate(next(init))(next))

    //Task 5
    @tailrec
    def drop[A](stream: Stream[A])(n: Int): Stream[A] = (stream, n) match
      case (Cons(_, t), n) if n != 0 => drop(t())(n - 1)
      case (stream, 0) => stream
      case _ => Empty()

    //Task 6
    def constant[A](c: A): Stream[A] = iterate(c)(c => c)

    //Task 7
    def fibs: Stream[Int] =
      def fibs(n1: Int, n2: Int): Stream[Int] = cons(n1, fibs(n2, n1 + n2))

      fibs(0, 1)

  end Stream