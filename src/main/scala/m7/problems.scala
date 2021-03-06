package m7

import example.Hello.compress

import scala.annotation.tailrec

object m7 extends App {

  //1 - Get last item
  @tailrec
  def myLast[A](lst:List[A]):A = lst match {
    case head :: Nil => head
    case _ :: tail => myLast(tail)
  }

  //2 Get penultimate elem
  @tailrec
  def penultimate[A](lst:List[A]):A = lst match {
    case head :: List(tail) => head
    case _ :: tail => penultimate(tail)
  }

  // 3 Find elem at N
  @tailrec
  def findAtN[A](lst:List[A], n:Int):A = (n,lst) match {
    case (0, h::_) => h
    case (n, _::tail) if n > 0 => findAtN(tail, n - 1)

  }

  // 4 Length of list

  def length[A](l:List[A]):Int = {
    @tailrec
    def lengthN[A](n:Int, l:List[A]):Int = l match {
      case Nil => n
      case _::tail => lengthN(n + 1, tail)
    }
    lengthN(0,l)
  }

  // Using foldRight & foldLeft

  def findAtNR[A](lst:List[A]):Int = lst.foldRight(0)((_,x)=>x+1)

  def findAtNL[A](lst:List[A]):Int = lst.foldLeft(0)((x,_)=>x+1)

  // 5 Reverse a list

  def reverse[A](lst: List[A]): List[A] = {
    @tailrec
    def iter(acc: List[A], lst: List[A]): List[A] = lst match {
      case Nil => acc
      case head :: tail => iter(head :: acc, tail)
    }
    iter(Nil, lst)
  }

  // 6 Palindrome

  def palindrome[A](lst:List[A]):Boolean = {
    true
  }

  // 7 Flatten List (Use nested lista)
  /*
    sealed trait NestedLista [+A]
    case class Elemento[A](a:A) extends NestedLista[A]
    case class Lista[A](lst:List[NestedLista[A]]) extends NestedLista[A]

    val nl1 = Lista(List(Elemento(1),Lista(List(Elemento(2))),Elemento(3)))

    def myFlatten[A](lst:List[List[A]]):List[A] = {
      def iter[A](lst:List[Lista[A]], acc: List[A]): List[A] = lst match {
        case Nil => acc
        case Elemento(n) :: xs => iter(xs, n :: acc)
        case Lista(x) :: xs => iter(x)
      }
    }
  */

  // 8 Eliminate duplicate consecutive

  def compress[A](lst:List[A]):List[A] = lst match {
    case Nil => Nil
    case head::Nil => List(head)
    case head::tail if (head == tail.head) => compress(tail)
    case head::tail => head::compress(tail)
  }

  // Alexander refactor
  def compress2[A](lst:List[A]):List[A] = lst match {
    case Nil => Nil
    case head::Nil => List(head)
    case head::tail => if (head == tail.head) compress2(tail) else head::compress2(tail)
  }


  def pack[A](lst: List[A]):List[List[A]] = {
    @tailrec
    def iter(lst: List[A], lstAcc:List[List[A]], lst1:List[A]):List[List[A]] = lst match {
      case head :: Nil => lstAcc ::: List(head :: lst1)
      case head :: middle :: tail => if (head == middle) iter(middle :: tail, lstAcc, head :: lst1) else iter(middle :: tail, lstAcc ::: List(head :: lst1), Nil)
    }
    iter(lst,List(),Nil)
  }

  def encode[A](lst: List[A]):List[List[A]] = {
    @tailrec
    def iter(lst: List[A], lstAcc:List[List[A]], lst1:List[A]):List[List[A]] = lst match {
      case head :: Nil => lstAcc ::: List(head :: lst1)
      case head :: middle :: tail => if (head == middle) iter(middle :: tail, lstAcc, head :: lst1) else iter(middle :: tail, lstAcc ::: List(head :: lst1), Nil)
    }
    iter(lst,List(),Nil)
  }

  sealed trait Valores[+A]
  case class Multiple[A](veces:Int, unA: A) extends Valores[A]
  case class Unico[A](unA: A) extends Valores[A]

  val lst = List(1,2,3,4,5,6,7,8)
  val lst2 = List(1,1,1,2,2,2,3,4,4,4,4)
  val lst3 = List("a","a","a","b","b","a","c")
  println(pack(lst3))
}


