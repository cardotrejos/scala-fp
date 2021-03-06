package example

import scala.annotation.tailrec

trait Felino {
  def color():String = "Amarillo mostaza"
  def sonido():String
}

case class Leon(val melena:Int = 0) extends Felino {
  override def color(): String = "Blanco"
  def sonido(): String = "Chrzzzz"
  def tamano():Int = 2
}

case class Tigre(val melena:Int = 0) extends Felino {
  override def color(): String = "Mostaza"
  def sonido(): String = "Plop"
}

case class Gato(val comidaFav:String = "Lasagna") extends Felino {
  override def color(): String = "Salmon"
  def sonido(): String = "Miau"
}

trait Forma {
  def tamano():Int
  def perimetro():Double
  def area():Double
}

trait Rectangular extends Forma {
  def tamano():Int = 4
}

case class Circular(val radio:Double = 1) extends Forma{
  def tamano():Int = 0
  def perimetro():Double = 2*math.Pi*radio
  def area():Double = math.Pi* radio * radio
}

case class Rectangulo(val lado1:Double, val lado2:Double) extends Rectangular {
  def perimetro():Double = lado1 * 2 + lado2 * 2
  def area():Double = lado1 * lado2
}

case class Cuadrado(val lado:Double) extends Rectangular {
  def perimetro():Double = lado * tamano()
  def area():Double = lado * lado
}

object Main extends App {
  val tigre1 = new Tigre
  // println(tigre1.color())

  //val lst = List(1,2,3,4,5,6)

  // println(lst.splitAt(2) match {case (l1,l2) => l1 ::: l2.tail})


  def subs[A](lst:List[A]):List[List[A]] =  {
    val a = lst match {
      case Nil => List(Nil)
      case head :: tail => subs(tail).map(head :: _) ::: subs(tail)
    }
    a.sortBy(_.length)
  }

  val lst2 = List(1,2,3)

  def permutations(lst: List[Int]): List[List[Int]] = lst match {
    case Nil => Nil
    case List(x) => List(List(x))
    case _ => lst.flatMap(x => permutations(lst.filterNot(_==x)).map(p => x :: p))
  }
