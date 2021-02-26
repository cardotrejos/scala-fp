import scala.io.StdIn

object Main extends App {

    def readValues(prompt:String):Double = {
        val s = StdIn.readLine(prompt)
        val value = s.toDouble
        value
    }
    
    val bono = 0.30
    def calcSalarioBonoClausura(salary:Double, deductions:Double) {
        val calculo = salary * bono - deductions
        calculo
    }

    def genCalSalarioBono(bonus:Double):(Double, Double) => Double =  bonus match {
        case 5 => (salary:Double, deductions:Double) => {(salary * 1.05 - deductions)}
        case 20 => (salary:Double, deductions:Double) => {(salary * 1.20 - deductions)}
    }

    val calSalario15 = (Double,Double)=>Double = { genCalSalarioBono(0.15) }
    
    val calcSalario5 = genSalarioBono(5) // Ejercicio 7

    val calcSalario20 = genSalarioBono(20) // Ejercicicio 8

    val salary = readValues("Ingrese su salario ")

    val deductions = readValues("Ingrese sus deducciones ")

    val calcSalario = (salary:Double, deductions:Double) =>  {salary - deductions}
    
    val calcSalarioBono = (salary:Double, deductions:Double) =>  {salary * 1.10 - deductions}

    val compSalario = (f:(Double, Double) => Double, salary:Double, deductions:Double) =>  f(salary, deductions)


    println("El salario es: " + compSalario(calcSalario, salary, deductions))
    println("Los bonos con 5% son: " + calcSalario5(salary, deductions))
    println("Los bonos con 20% son: " + calcSalario20(salary, deductions))
    println("El salario es: " + compSalario(calcSalarioBonoClausura, salary, deductions)) // Ejercicio 10
 }