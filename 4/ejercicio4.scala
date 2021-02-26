import scala.io.StdIn

object Main extends App {

    def readValues(prompt:String):Double = {
        val s = StdIn.readLine(prompt)
        val value = s.toDouble
        value
    }

    val salary = readValues("Ingrese su salario ")

    val deductions = readValues("Ingrese sus deducciones ")
    
    val calcSalarioBono = (salary:Double, deductions:Double) =>  {salary * 1.10 - deductions}

    println("El salario con bonos menos deducciones es: " + calcSalarioBono(salary, deductions))
 }


