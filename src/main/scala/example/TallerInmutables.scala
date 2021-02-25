package example

object TallerInmutables extends App {

  sealed trait List [+A]
  case object Nil extends List[Nothing]
  case class Const[+A](h:A,t:List[A]) extends List[A]

  object List {

    def length[A](lst:List[A]):Int= lst match {
      case Nil => 0
      case Const(h,t) => 1 + length(t)}

    def sum(ints: List[Int]):Int = ints match {
      case Nil => 0
      case Const(h,t) => h + sum(t)
    }

    def product(ds: List[Double]):Double = ds match {
      case Nil => 1
      case Const(h,t) => h * product(t)
    }

    def apply[A](as: A*) : List[A] ={
      if (as.isEmpty) Nil
      else Const(as.head, apply(as.tail: _*))
    }

    //Ejercicio 1
    val x = List(4,5,6,7,8) match {
      case Const(x, Const(5, Const(7, _)))  => x
      case Nil  => 1
      case Const(x, Const(y, Const(6, Const(7, _))))  => x + y
      case Const(h, t)  => h + sum(t )
      case _  => 777
    }

    // Ejercicio 2
    def tail[A](lst:List[A]):List[A] = lst match {
      case Nil => Nil
      case Const(h,Nil) => Nil
      case Const(h,t) => t
    }

    //Ejercicio 3
    def head[A](lst:List[A]):A = lst match {
      case Const(h,t) => h
    }

    //Ejercicio 4
    def and(lst:List[Boolean]):Boolean = lst match {
      case Const(true,Nil) => true
      case Const(true,t) => and(t)
      case Const(false,_) => false
    }

    //Ejercicio 5
    def or(lst:List[Boolean]):Boolean = lst match {
      case Nil => true
      case Const(false,Nil) => false
      case Const(false,t) => or(t)
      case Const(true, t) => true
    }

  }
}