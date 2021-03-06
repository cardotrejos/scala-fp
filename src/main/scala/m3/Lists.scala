package example

import scala.annotation.tailrec

sealed trait List[+A]

case object Nil extends List[Nothing]

case class Const[+A](h: A, t: List[A]) extends List[A]

object List {

  def length[A](lst: List[A]): Int = lst match {
    case Nil => 0
    case Const(h, t) => 1 + length(t)
  }

  def sum(ints: List[Int]): Int = ints match {
    case Nil => 0
    case Const(h, t) => h + sum(t)
  }

  def product(ds: List[Double]): Double = ds match {
    case Nil => 1
    case Const(h, t) => h * product(t)
  }

  def apply[A](as: A*): List[A] = {
    if (as.isEmpty) Nil
    else Const(as.head, apply(as.tail: _*))
  }

  def tail[A](lst: List[A]): List[A] = lst match {
    case Nil => Nil
    case Const(h, t) => t
  }

  def head[A](lst: List[A]): A = lst match {
    case Const(h, t) => h
  }

  def and(lst: List[Boolean]): Boolean = lst match {
    case Nil => true
    case Const(h, Nil) => h
    case Const(h, t) => h && and(t)
  }

  def or(lst: List[Boolean]): Boolean = lst match {
    case Nil => false
    case Const(h, Nil) => h
    case Const(h, t) => h || or(t)
  }

  def max(lst: List[Int]): Int = {
    @tailrec
    def maxr(lst: List[Int], max: Int): Int = lst match {
      case Nil => max
      case Const(h, t) => maxr(t, if (h > max) h else max)
    }

    maxr(tail(lst), head(lst))
  }

  def min(lst: List[Int]): Int = {
    @tailrec
    def minr(lst: List[Int], min: Int): Int = lst match {
      case Nil => min
      case Const(h, t) => minr(t, if (h < min) h else min)
    }

    minr(tail(lst), head(lst))
  }

  def minMax(lst: List[Double]): (Double, Double) = {
    def selectionmm(op: Int, n1: Double, n2: Double): Double = op match {
      case 1 => if (n1 > n2) n1 else n2
      case _ => if (n1 < n2) n1 else n2
    }

    @tailrec
    def minMaxR(lst: List[Double], mm: (Double, Double)): (Double, Double) = lst match {
      case Nil => mm
      case Const(h, t) => minMaxR(t, (selectionmm(0, h, mm._1), selectionmm(1, h, mm._2)))
    }

    minMaxR(tail(lst), (head(lst), head(lst)))
  }

  def addEnd[A](h: A, t: List[A]): List[A] = t match {
    case Nil => Const(h, Nil)
    case _ => Const(head(t), addEnd(h, tail(t)))
  }

  def append[A](lst1: List[A], lst2: List[A]): List[A] =
    (lst1, lst2) match {
      case (Nil, Nil) => Nil
      case (lst1, Nil) => lst1
      case (Nil, lst2) => lst2
      case (Const(h, t), lst2) => Const(h, append(t, lst2))
    }

  @tailrec
  def drop[A](n: Int, lst: List[A]): List[A] = (n, lst) match {
    case (0, lst) => lst
    case (n, Nil) => Nil
    case (n, Const(h, t)) => drop(n - 1, t)
  }

  // Ejercicio 1
  def take[A](n: Int, lst: List[A]): List[A] = {
    @tailrec
    def iter[A](n: Int, lst: List[A], acc: List[A]): List[A] = (n, lst) match {
      case (0, lst) => acc
      //case (n, Nil) => Nil
      case (n, Const(h, t)) => iter(n - 1, t, addEnd(h, acc))
    }

    iter(n, lst, Nil)
  }

  // Ejercicio 2

  def init[A](lst: List[A]): List[A] = lst match {
    case Const(h, Nil) => Nil
    case Const(h, t) => Const(h, init(t))
  }

  //Ejercicio 3
  def split[A](n: Int, lst: List[A]): (List[A], List[A]) = {
    @tailrec
    def iter[A](n: Int, lst: List[A], acc: List[A]): (List[A], List[A]) = (n, lst) match {
      case (0, lst) => (acc, lst)
      //case (n, Nil) => (Nil, Nil)
      case (n, Const(h, t)) => iter(n - 1, t, addEnd(h, acc))
    }

    iter(n, lst, Nil)
  }

  // Ejercicio 4 zip
  def zip[A, B](lst1: List[A], lst2: List[B]): List[(A, B)] = (lst1, lst2) match {
    case (Nil, _) => Nil
    case (_, Nil) => Nil
    case (Const(h1, t1), Const(h2, t2)) => Const((h1, h2), zip(t1, t2))
  }

  // Ejercicio 5 unzip
  def unzip[A, B](lst: List[(A, B)]): (List[A], List[B]) = {
    @tailrec
    def iter[A, B](lst: List[(A, B)], acc: (List[A], List[B])): (List[A], List[B]) = lst match {
      case Nil => acc
    }
  }

  // Ejercicio 6 reverse


  //Ejercicio 7 concat

  def foldRight[A, B](lst: List[A], z: B)(f: (A, B) => B): B =
    lst match {
      case Nil => z
      case Const(h, t) => f(h, foldRight(t, z)(f))
    }

  //Ejercicio 14
  def length2[A](lst: List[A]): Int = {
    foldRight(lst, 0)((_, x) => x + 1)
  }

  //Ejercicio 15
  def andR(lst: List[Boolean]): Boolean = {
    foldRight(lst, true)(_ && _)
  }

  //Ejercicio 16
  def takeWhile[A](lst: List[A])(p: A => Boolean): List[A] = {
    foldRight(lst, Nil: List[A])((h, t) => if (p(h)) Const(h, t) else Nil)
  }

  //Ejercicio 17
  def filter[A](lst: List[A])(p: A => Boolean): List[A] = {
    foldRight(lst, Nil: List[A])((h, t) => if (p(h)) Const(h, t) else t)
  }

  //Ejercicio 18
  def unzipR[A, B](lst: List[(A, B)]): (List[A], List[B]) =
    foldRight(lst, (Nil: List[A], Nil: List[B]))((x, y) => (Const(x._1, y._1), Const(x._2, y._2)))

  @tailrec
  def foldLeft[A, B](lst: List[A], z: B)(f: (B, A) => B): B =
    lst match {
      case Const(h, t) => foldLeft(t, f(z, h))(f)
      case Nil => z
    }

  //Ejercicio 19
  def lengthL[A](lst: List[A]): Int = {
    foldLeft(lst, 0)((y, _) => y + 1)
  }

  //Ejercicio 20
  def andL(lst: List[Boolean]): Boolean = {
    foldLeft(lst, true)(_ && _)
  }

  //Ejercicio 21
  def takeWhileL[A](lst: List[A])(p:A=>Boolean):List[A] = {
    foldLeft(List.reverse(lst),Nil:List[A])((y,x) => if(p(x)) Const(x,y) else Nil)
  }

  //Ejercicio 22
  def filterL[A](lst: List[A])(p: A => Boolean): List[A] = {
    foldRight(lst, Nil: List[A])((lst, e) => if (p(lst)) addEnd(lst, e) else e)
  }

  println(filterL(List(1, 2, 3, 4, 5, 6, 7, 8))(_ % 2 == 0))

  //Ejercicio 23
  def unzipL[A, B](lst: List[(A, B)]): (List[A], List[B]) =
    foldLeft(lst, (Nil: List[A], Nil: List[B]))((x, y) => (Const(y._1, x._1), Const(y._2, x._2)))

}