package lab03

import scala.:+
import scala.annotation.tailrec

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




  val l = List.Cons(10, List.Cons(20, List.Cons(30, List.Nil())))
  println(List.sum(l)) // 60

  import List.*

  println(sum(map(filter(l)(_ >= 20))(_ + 1))) // 21+31 = 52
