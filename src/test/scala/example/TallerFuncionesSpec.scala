package example

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import scala.math.Pi

class TallerFuncionesSpec extends AnyFlatSpec with Matchers {
  "1 El area de un triangulo de lados 5 y 6 " should " 15 " in {
    TallerFunciones.areaTrianguloRectangulo(5.0, 6.0) shouldEqual 15
  }
  "1 El area de un triangulo de lados 3 y 7 " should " 10.5 " in {
    TallerFunciones.areaTrianguloRectangulo(3.0, 7.0) shouldEqual 10.5
  }
  "2 El area de un circulo de radio 1 " should " Pi " in {
    TallerFunciones.areaDeUnCirculo(1) shouldEqual Pi
  }
  "2 El area de un circulo de radio 4 " should " 50.2654824 " in {
    TallerFunciones.areaDeUnCirculo(4) shouldBe 50.2654824 +- 0.0001
  }
  "3 La funcion calcSalario con 100 y 0 " should " 100.0 " in {
    TallerFunciones.calcSalario(100.0, 0.0) shouldBe 100.0 +- 0.0001
  }
  "3 La funcion calcSalario con 1000 y 770 " should " 230.0 " in {
    TallerFunciones.calcSalario(1000.0, 770.0) shouldBe 230.0 +- 0.0001
  }
  "4 La funcion calcSalarioBono con 100 y 0 " should " 110.0 " in {
    TallerFunciones.calcSalarioBono(100.0, 0.0) shouldBe 110.0 +- 0.0001
  }
  "4 La funcion calcSalarioBono con 100 y 50 " should " 60.0 " in {
    TallerFunciones.calcSalarioBono(100.0, 50.0) shouldBe 60.0 +- 0.0001
  }
  "5 La funcion compSalario con la funcion calSalario 100 y 0 " should " 100.0 " in {
    TallerFunciones.compSalario(TallerFunciones.calcSalario, 100.0, 0.0) shouldBe 100.0 +- 0.0001
  }
  "5 La funcion compSalario con la funcion calSalario 100 y 40 " should " 70.0 " in {
    TallerFunciones.compSalario(TallerFunciones.calcSalarioBono, 100.0, 40.0) shouldBe 70.0 +- 0.0001
  }
  "6 La funcion genCalSalarioBono con un bono de 0.05 y 100,0" should " 100.0 " in {
    TallerFunciones.genCalSalarioBono(0.05)(100, 0) shouldBe 105.0 +- 0.0001
  }
  "6 La funcion genCalSalarioBono con un bono de 0.5 y 100,30" should " 120.0 " in {
    TallerFunciones.genCalSalarioBono(0.5)(100, 30) shouldBe 120.0 +- 0.0001
  }
  "7 La funcion calcSalario5 con 100, 30 " should " 75.0 " in {
    TallerFunciones.calcSalario5(100, 30) shouldBe 75.0 +- 0.0001
  }
  "8 La funcion calcSalario20 con 100, 30 " should " 90.0 " in {
    TallerFunciones.calcSalario20(100, 30) shouldBe 90.0 +- 0.0001
  }
  "9 La funcion calcSalarioBonoClausura con 200, 50 cuando la variable bono esta definida" +
    "como bono = 1.3 " should " 210.0 " in {
    TallerFunciones.calcSalarioBonoClausura(200, 50) shouldBe 210.0 +- 0.0001
  }
  "11 La funcion calSalario15 con 100, 0 " should " 115.0 " in {
    TallerFunciones.calSalario15(100, 0) shouldBe 115.0 +- 0.0001
  }
  "12 La funcion calSalario100 con 100, 100 " should " 100.0 " in {
    TallerFunciones.calSalario100(100, 100) shouldBe 100.0 +- 0.0001
  }
  "15 El factorial de 3" should " 6 " in {
    TallerFunciones.factorial(3) shouldEqual 6
  }
  "15 El fibonacci  7" should " 5040 " in {
    TallerFunciones.factorial(7) shouldEqual 5040
  }
  "16 El fibonacci 2 " should " 1 " in {
    TallerFunciones.fibonacci(2) shouldEqual 1
  }
  "16 El fibonacci 2 " should " 13 " in {
    TallerFunciones.fibonacci(7) shouldEqual 13
  }
  "17 El factorial de 7" should " 5040 " in {
    TallerFunciones.tailFactorial(7) shouldEqual 5040
  }
  "17 El factorial de 5" should " 120 " in {
    TallerFunciones.tailFactorial(5) shouldEqual 120
  }
}
