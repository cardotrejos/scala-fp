package example

sealed trait Nat

case object Cero extends Nat

case class Succ(nat: Nat) extends Nat

object Nat {

  //Ejercicio 10
  def fromNatToInt(nat: Nat): Int = nat match {
    case Cero => 0
    case Succ(nat) => 1 + fromNatToInt(nat)
  }

  //Ejercicio 11 / Int >= 0

  def fromIntToNat(i: Int): Nat = i match {
    case 0 => Cero
    case n => Succ(fromIntToNat(n - 1))
  }
}