package clases

object comp {
    def cuadrado(value:Float):Float = {
    val total = value * value
    total
  }
  def cubo(value:Double):Double = {
    val total = math.pow(value, 3)
    total
  }
}

object comp2 {
  def cuadrado(value: Long): Long = {
    val total = value * value
    total
  }
}

object prueba {
  def x = {
    println("x")
    1
  }

  val y = {
    println("y")
    x+2
  }

  def z = {
    println("z")
    x
    x+"c"
  }
}


class Gato (val nombre:String, val color:String, val comida:String) {

}

object VentaDeChurrus{

  def despachar(val gato: Gato) = {
    
  }

}

class Conductor (val nombre:String, val apellido:String, val totalCarreras:Int, val carrerasTerminadas:Int) {
  def getCarrerasNoTerminadas:Int = {
    val noTerminadas = totalCarreras - carrerasTerminadas
    noTerminadas
  }
}

class Escuderia (val nombre:String, val conductor: Conductor) {

}

class Sumador(monto:Int) {
  def adicionar(valor:Int): Int = valor + monto
}

class Persona(val nombre:String, val apellido:String){
  def nombreCompleto = s"$nombre $apellido"
}

object Persona {
  def apply(fullName:String): Persona = {
    val completeName = fullName.split(" ")
    val lastName = completeName(1)
    val firstName = completeName(0)
    new Persona(firstName, lastName)
    // new Persona(completeName(0), completeName(1))
  }
}

class Director(val nombre:String, val apellido:String, val nacimiento:Int) {
  def nombreCompleto:String = s"$nombre $apellido"
  def copy(nombre:String = this.nombre, apellido:String = this.apellido, nacimiento:Int = this.nacimiento):Director =
    new Director(nombre, apellido, nacimiento)
}

object Director {
  def apply(nombre:String, apellido:String, nacimiento:Int): Director = {
    new Director(nombre, apellido, nacimiento)
  }

  def esMayor(director1:Director, director2: Director):Director = {
    val mayor = if (director1.nacimiento < director2.nacimiento) director1 else director2
    mayor
  }
}

class Pelicula(val nombre:String, val presentacion:Int, val rangoIMDB:Double, val director: Director) {
  def directorEdad: Int = presentacion - director.nacimiento
  def esDirigidaPor(director: Director): Boolean = this.director == director
  def copy(nombre:String = this.nombre, presentacion:Int = this.presentacion, rangoIMDB:Double = this.rangoIMDB, director: Director):Pelicula =
    new Pelicula(nombre, presentacion, rangoIMDB, director)
}

object Pelicula {
  def apply(nombre:String,presentacion:Int, rangoIMDB:Double, director: Director):Pelicula = {
    new Pelicula(nombre, presentacion, rangoIMDB, director)
  }

  def mejorCalificada(pelicula1: Pelicula, pelicula2: Pelicula): Pelicula = {
    val mejor = if (pelicula1.rangoIMDB > pelicula2.rangoIMDB) pelicula1 else pelicula2
    mejor
  }

  def mayorDirectorEnElTiempo(pelicula1: Pelicula, pelicula2: Pelicula):Director = {
    val edadPresentacion1 = pelicula1.presentacion - pelicula1.director.nacimiento
    val edadPresentacion2 = pelicula2.presentacion - pelicula2.director.nacimiento

    val mayor = if(edadPresentacion1 > edadPresentacion2) pelicula1.director else pelicula2.director
    mayor
  }

}