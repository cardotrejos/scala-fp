package example
import scala.annotation.tailrec
import scala.math.Pi

object TallerFunciones {

  // Ejercicio 1
  val areaTrianguloRectangulo = (base:Double, height:Double) =>  {(base*height)/2}

  // Ejercio 2
  val areaDeUnCirculo = new Function1[Double, Double] {
    def apply(radio:Double): Double = {Pi * radio * radio}
  }

  // Ejercicio 3
  val calcSalario = (salary:Double, deductions:Double) =>  {salary - deductions}

  // Ejercicio 4
  val calcSalarioBono = (salary:Double, deductions:Double) =>  {salary * 1.10 - deductions}

  // Ejercicio 5
  val compSalario = (f:(Double, Double) => Double, salary:Double, deductions:Double) =>  f(salary, deductions)

  //Ejercicio 6
  def genCalSalarioBono(bonus:Double):(Double, Double) => Double =  bonus match {
    case 5 => (salary:Double, deductions:Double) => {(salary * 1.05 - deductions)}
    case 20 => (salary:Double, deductions:Double) => {(salary * 1.20 - deductions)}
    case _ => (salary:Double, deductions:Double) => { salary *(1 + bonus ) - deductions }
  }

  //Ejercicio 7
  val calcSalario5 = genCalSalarioBono(5)

  //Ejercicio 8
  val calcSalario20 = genCalSalarioBono(20)

  //Ejercicio 9
  val bono = 1.30
  def calcSalarioBonoClausura(salary:Double, deductions:Double) = {
    val calculo = salary * bono - deductions
    calculo
  }
  //Ejercicio 10
  // See test

  //Ejercicio 11
  val calSalario15 = genCalSalarioBono(0.15)

  //Ejercicio 12
  val calSalario100 = genCalSalarioBono(1.00)

  //Ejercicio 13
  def genCalSalarioBono2(salary:Double, deductions:Double):(Double) => Double =
    (bonus:Double) => { salary*(1+bonus) - deductions }

  //Ejercicio 14
  // Pendiente de corregir

  //Ejercicio 15
    def factorial(n:Int):Int = {
      if (n==0) 1
      else if (n==1) 1
      else n * factorial(n-1)
    }

  //Ejercicio 16
  def fibonacci(n:Int):Int = {
    if (n==0) 0
    else if (n==1) 1
    else fibonacci(n-1) + fibonacci(n-2)
  }

  // Ejercicio 17
  def tailFactorial(n:Int):Int = {
    @tailrec
    def iter(n:Int, result:Int):Int =
      if (n==1) result
      else iter(n-1,result * n)
    iter(n, 1)
  }

}
