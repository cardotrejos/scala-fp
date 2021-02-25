import scala.io.StdIn

object Main extends App {

    def readValues(prompt:String):Double = {
        val s = StdIn.readLine(prompt)
        val value = s.toDouble
        value
    }

    val salary = readValues("Ingrese su salario ")

    val deductions = readValues("Ingrese sus deducciones ")
    
    val calcSalario = (salary:Double, deductions:Double) =>  {salary - deductions}

    println("El salario es: " + calcSalario(salary, deductions))
 }


