import scala.io.StdIn

object  Main extends App {

    def readValues(prompt:String):Int = {
        val s = StdIn.readLine(prompt)
        val int = s.toInt
        int
    }

    val base = readValues("Ingrese base ")

    val height = readValues("Ingrese altura ")
    
    val areaTrianguloRectangulo = (base:Int, height:Int) =>  {(base*height)/2}

    println("El area del triangulo es: " + areaTrianguloRectangulo(base, height))
 }


