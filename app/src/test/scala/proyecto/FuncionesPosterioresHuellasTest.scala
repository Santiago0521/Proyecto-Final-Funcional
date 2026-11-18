package proyecto

import org.scalatest.funsuite.AnyFunSuite
import org.junit.runner.RunWith
import org.scalatestplus.junit.JUnitRunner
import Similitud._

@RunWith(classOf[JUnitRunner])
class FuncionesPosterioresHuellasTest extends AnyFunSuite {

  test("jaccard: huellas iguales tienen similitud uno") {
    val a: Huella = Set(Vector("a", "b"), Vector("b", "c"))
    assert(jaccard(a, a) == 1.0)
  }

  test("jaccard: huellas disjuntas tienen similitud cero") {
    val a: Huella = Set(Vector("a", "b"))
    val b: Huella = Set(Vector("x", "y"))
    assert(jaccard(a, b) == 0.0)
  }

  test("jaccard: dos huellas vacías tienen similitud cero") {
    assert(jaccard(Set.empty, Set.empty) == 0.0)
  }

  test("jaccard: una huella contenida en otra usa la unión completa") {
    val a: Huella = Set(Vector("a"), Vector("b"))
    val b: Huella = Set(Vector("a"), Vector("b"), Vector("c"), Vector("d"))
    assert(jaccard(a, b) == 0.5)
  }

  test("jaccard: calcula la intersección sobre la unión con solapamiento parcial") {
    val a: Huella = Set(Vector("a"), Vector("b"), Vector("c"))
    val b: Huella = Set(Vector("b"), Vector("c"), Vector("d"))
    assert(math.abs(jaccard(a, b) - 0.5) < 1e-9)
  }

  test("parejas: cero documentos producen un vector vacío") {
    assert(parejas(0).isEmpty)
  }

  test("parejas: un documento no forma parejas consigo mismo") {
    assert(parejas(1).isEmpty)
  }

  test("parejas: dos documentos producen una sola pareja") {
    assert(parejas(2) == Vector((0, 1)))
  }

  test("parejas: tres documentos producen todas las parejas ordenadas") {
    assert(parejas(3) == Vector((0, 1), (0, 2), (1, 2)))
  }

  test("parejas: cuatro documentos producen seis parejas sin repeticiones") {
    assert(parejas(4) == Vector(
      (0, 1), (0, 2), (0, 3), (1, 2), (1, 3), (2, 3)))
  }

  test("similitudes: devuelve una medición por pareja en el orden recibido") {
    val hs: Vector[Huella] = Vector(
      Set(Vector("a"), Vector("b")),
      Set(Vector("a"), Vector("c")),
      Set(Vector("x")))
    val ps = Vector((1, 2), (0, 1))
    val resultado = similitudes(hs, ps)
    assert(resultado.map(m => (m._1, m._2)) == ps)
    assert(resultado.map(_._3) == Vector(0.0, 1.0 / 3.0))
  }

  test("similitudes: dos huellas idénticas producen similitud uno") {
    val h: Huella = Set(Vector("uno", "dos"))
    assert(similitudes(Vector(h, h), Vector((0, 1))) == Vector((0, 1, 1.0)))
  }

  test("similitudes: huellas disjuntas producen similitud cero") {
    val hs: Vector[Huella] = Vector(Set(Vector("a")), Set(Vector("b")))
    assert(similitudes(hs, Vector((0, 1))) == Vector((0, 1, 0.0)))
  }

  test("similitudes: una lista de parejas vacía produce resultado vacío") {
    val hs: Vector[Huella] = Vector(Set(Vector("a")), Set(Vector("b")))
    assert(similitudes(hs, Vector.empty).isEmpty)
  }

  test("similitudes: dos huellas vacías se miden como cero") {
    val hs: Vector[Huella] = Vector(Set.empty, Set.empty)
    assert(similitudes(hs, Vector((0, 1))) == Vector((0, 1, 0.0)))
  }

  test("indiceInvertido: un corpus sin documentos produce un mapa vacío") {
    assert(indiceInvertido(Vector.empty).isEmpty)
  }

  test("indiceInvertido: huellas vacías no agregan fragmentos al mapa") {
    assert(indiceInvertido(Vector(Set.empty, Set.empty)).isEmpty)
  }

  test("indiceInvertido: un fragmento apunta a la posición de su documento") {
    val f: Fragmento = Vector("a", "b")
    assert(indiceInvertido(Vector(Set(f))) == Map(f -> Set(0)))
  }

  test("indiceInvertido: un fragmento compartido guarda ambas posiciones") {
    val f: Fragmento = Vector("a", "b")
    assert(indiceInvertido(Vector(Set(f), Set(f))) == Map(f -> Set(0, 1)))
  }

  test("indiceInvertido: varios fragmentos registran sus documentos correspondientes") {
    val f: Fragmento = Vector("a", "b")
    val g: Fragmento = Vector("b", "c")
    val h: Fragmento = Vector("x", "y")
    val hs: Vector[Huella] = Vector(Set(f, g), Set(f), Set(h, g))
    assert(indiceInvertido(hs) == Map(
      f -> Set(0, 1),
      g -> Set(0, 2),
      h -> Set(2)))
  }
}
