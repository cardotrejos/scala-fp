package example

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import scala.math.Pi

class TallerFuncionesSpec extends AnyFlatSpec with Matchers {
  "El area del triangulo con base 7 y altura 8" should "be 28" in {
    TallerFunciones.areaTrianguloRectangulo(7,8) shouldEqual 28
  }

  "El area del triangulo con base 10 y altura 8" shou

  "El area del circulo con radio 1" should "be Pi" in {
    TallerFunciones.areaDeUnCirculo(1) shouldEqual Pi


  }
}
