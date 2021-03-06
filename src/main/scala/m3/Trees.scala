package example

import scala.annotation.tailrec

sealed trait Tree[+A]
case class Leaf[A](value:A) extends Tree[A]
case class Branch[A](left:Tree[A],right:Tree[A]) extends Tree[A]

object Trees extends App {

  //Ejercicio 11
  def size[A](tree: Tree[A]):Int =
    tree match {
      case Leaf(_) => 1
      case Branch(l,r) => 1 + size(l) + size(r)
    }

  def t = Branch(Branch(Leaf(1), Leaf(2)), Leaf(3))
  println(size(t))

  def maximum(tree: Tree[Int]): Int = tree match {
    case Leaf(n) => n
    case Branch(l, r) => maximum(l) max maximum(r)
  }

  //Ejercicio 12
  def depth[A](tree: Tree[A]):Int =
    tree match {
      case Leaf (_) => -2
      case Branch(l, r) => 2 + (depth(l) max depth(r))
    }

}