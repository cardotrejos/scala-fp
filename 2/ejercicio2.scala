import scala.io.StdIn
import scala.math.Pi

object Main extends App {

    def readValues(prompt:String):Int = {
        val s = StdIn.readLine(prompt)
        val int = s.toInt
        int
    }

    val radio = readValues("Ingrese radio ")
  
    val areaDeUnCirculo = new Function1[Double, Double] { 
        def apply(radio:Double) = {Pi * radio * radio} 
    }

    println("El area del circulo es: " + areaDeUnCirculo(radio))
 }


