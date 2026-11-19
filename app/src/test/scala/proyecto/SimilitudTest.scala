package proyecto

import org.scalatest.funsuite.AnyFunSuite
import org.junit.runner.RunWith
import org.scalatestplus.junit.JUnitRunner
import Similitud._

/**
 * Pruebas de contrato de la versión secuencial: una por función, con los
 * valores de los dos ejemplos del enunciado. No se modifican.
 *
 * La rúbrica pide además cinco casos propios por función, distintos de estos y
 * de los ejemplos del enunciado. Van al final de este archivo o en un archivo
 * nuevo de esta misma carpeta; las dos formas valen.
 */
@RunWith(classOf[JUnitRunner])
class SimilitudTest extends AnyFunSuite {

  val eps = 1e-9

  // Ejemplo 1 del enunciado, con las palabras ya normalizadas para que estas
  // pruebas no dependan unas de otras.
  val d0: Documento = ("d0", Vector("el", "gato", "duerme", "en", "el", "sofa"))
  val d1: Documento = ("d1", Vector("el", "gato", "duerme", "en", "la", "alfombra"))
  val d2: Documento = ("d2", Vector("la", "lluvia", "moja", "el", "parque"))
  val c1: Corpus = Vector(d0, d1, d2)

  // Ejemplo 2 del enunciado.
  val e0: Documento = ("e0", Vector("la", "funcion", "recursiva", "termina",
    "cuando", "llega", "al", "caso", "base"))
  val e1: Documento = ("e1", Vector("toda", "funcion", "recursiva", "termina",
    "cuando", "llega", "al", "caso", "base"))
  val e2: Documento = ("e2", Vector("el", "paralelismo", "divide", "el",
    "trabajo", "en", "tareas", "independientes"))
  val e3: Documento = ("e3", Vector("el", "paralelismo", "divide", "el",
    "trabajo", "en", "hilos", "independientes"))
  val c2: Corpus = Vector(e0, e1, e2, e3)

  test("normalizar: baja a minúsculas y parte en palabras") {
    assert(normalizar("El Gato Duerme") == Vector("el", "gato", "duerme"))
  }

  test("Propia normalizar 1: convierte letras mayúsculas a minúsculas") {
    assert(normalizar("HOLA MUNDO") == Vector("hola", "mundo"))
  }

  test("Propia normalizar 2: elimina signos de puntuación") {
    assert(normalizar("Hola, mundo. ¿Cómo estás?")
      == Vector("hola", "mundo", "cómo", "estás"))
  }

  test("Propia normalizar 3: conserva los números") {
    assert(normalizar("Scala 2026 version 3")
      == Vector("scala", "2026", "version", "3"))
  }

  test("Propia normalizar 4: convierte símbolos y espacios en separadores") {
    assert(normalizar("gato@perro#casa!sol")
      == Vector("gato", "perro", "casa", "sol"))
  }

  test("Propia normalizar 5: devuelve vacío cuando no hay palabras") {
    assert(normalizar("!!! ### ... 123_456")
      == Vector("123", "456"))
    assert(normalizar("   !!!   ") == Vector.empty)
  }

  test("fragmentos: son las ventanas consecutivas, en orden") {
    val ts = Vector("a", "b", "c", "d")
    assert(fragmentos(ts, 2) ==
      Vector(Vector("a", "b"), Vector("b", "c"), Vector("c", "d")))
  }

  test("Propia fragmentos 1: genera un único fragmento") {
    assert(fragmentos(Vector("a", "b", "c"), 3)
      == Vector(Vector("a", "b", "c")))
  }

  test("Propia fragmentos 2: genera fragmentos de una palabra") {
    assert(fragmentos(Vector("sol", "luna", "mar"), 1)
      == Vector(Vector("sol"), Vector("luna"), Vector("mar")))
  }

  test("Propia fragmentos 3: devuelve vacío si k supera el tamaño") {
    assert(fragmentos(Vector("a", "b"), 3) == Vector.empty)
  }

  test("Propia fragmentos 4: devuelve vacío para un vector vacío") {
    assert(fragmentos(Vector.empty, 2) == Vector.empty)
  }

  test("Propia fragmentos 5: conserva el orden y los repetidos") {
    assert(fragmentos(Vector("a", "b", "a", "c"), 2) ==
      Vector(Vector("a", "b"), Vector("b", "a"), Vector("a", "c")))
  }

  test("huella: el ejemplo 1 tiene 4, 4 y 3 fragmentos") {
    assert(huella(d0, 3).size == 4)
    assert(huella(d1, 3).size == 4)
    assert(huella(d2, 3).size == 3)
  }

  test("Propia huella 1: elimina fragmentos repetidos") {
    val d: Documento = ("d1", Vector("a", "b", "a", "b"))
    assert(huella(d, 2) == Set(Vector("a", "b"), Vector("b", "a")))
  }

  test("Propia huella 2: devuelve una huella vacía si k es mayor") {
    val d: Documento = ("d2", Vector("casa", "perro"))
    assert(huella(d, 3).isEmpty)
  }

  test("Propia huella 3: obtiene todos los fragmentos distintos") {
    val d: Documento = ("d3", Vector("rojo", "azul", "verde"))
    assert(huella(d, 2) ==
      Set(Vector("rojo", "azul"), Vector("azul", "verde")))
  }

  test("Propia huella 4: con k igual a uno obtiene palabras distintas") {
    val d: Documento = ("d4", Vector("sol", "luna", "sol"))
    assert(huella(d, 1) == Set(Vector("sol"), Vector("luna")))
  }

  test("Propia huella 5: un documento vacío produce una huella vacía") {
    val d: Documento = ("d5", Vector.empty)
    assert(huella(d, 2).isEmpty)
  }

  test("huellas: hay una huella por documento") {
    assert(huellas(c1, 3).length == 3)
  }

   test("Propia huellas 1: devuelve una huella por documento") {
    val corpus: Corpus = Vector(
      ("a", Vector("sol", "luna")),
      ("b", Vector("mar", "cielo")),
      ("c", Vector("perro", "gato"))
    )
    assert(huellas(corpus, 1).length == 3)
  }

  test("Propia huellas 2: conserva el orden de los documentos") {
    val corpus: Corpus = Vector(
      ("primero", Vector("a", "b")),
      ("segundo", Vector("x", "y")),
      ("tercero", Vector("m", "n"))
    )
    assert(huellas(corpus, 2) == Vector(
      Set(Vector("a", "b")),
      Set(Vector("x", "y")),
      Set(Vector("m", "n"))
    ))
  }

  test("Propia huellas 3: funciona con un corpus vacío") {
    assert(huellas(Vector.empty, 2) == Vector.empty)
  }

  test("Propia huellas 4: admite documentos con huellas vacías") {
    val corpus: Corpus = Vector(
      ("a", Vector("sol")),
      ("b", Vector("luna", "mar"))
    )
    assert(huellas(corpus, 3) == Vector(Set.empty, Set.empty))
  }

  test("Propia huellas 5: calcula correctamente huellas diferentes") {
    val corpus: Corpus = Vector(
      ("a", Vector("uno", "dos", "tres")),
      ("b", Vector("cuatro", "cinco", "seis"))
    )
    assert(huellas(corpus, 2) == Vector(
      Set(Vector("uno", "dos"), Vector("dos", "tres")),
      Set(Vector("cuatro", "cinco"), Vector("cinco", "seis"))
    ))
  }

  test("jaccard: ejemplo 2, las dos parejas dan 0.75 y 0.5") {
    assert(math.abs(jaccard(huella(e0, 3), huella(e1, 3)) - 0.75) < eps)
    assert(math.abs(jaccard(huella(e2, 3), huella(e3, 3)) - 0.5) < eps)
  }

  test("parejas: hay n (n - 1) / 2") {
    assert(parejas(5).length == 10)
  }

  test("similitudes: el ejemplo 1 da 1/3, 0 y 0") {
    val hs = huellas(c1, 3)
    val ms = similitudes(hs, parejas(3)).map(_._3)
    assert(math.abs(ms(0) - 2.0 / 6.0) < eps && ms(1) == 0.0 && ms(2) == 0.0)
  }

  test("indiceInvertido: el fragmento inicial de d0 está en d0 y en d1") {
    val idx = indiceInvertido(huellas(c1, 3))
    assert(idx(Vector("el", "gato", "duerme")) == Set(0, 1))
  }

  test("parejasCandidatas: en el ejemplo 2 quedan dos de seis") {
    val idx = indiceInvertido(huellas(c2, 3))
    assert(parejasCandidatas(idx) == Vector((0, 1), (2, 3)))
  }

  test("ranking: ordena de mayor a menor y trunca a p") {
    val ms: Vector[Medicion] = Vector((0, 1, 0.4), (0, 2, 0.1), (1, 2, 0.9))
    assert(ranking(ms, 0.0, 10).map(_._3) == Vector(0.9, 0.4, 0.1))
    assert(ranking(ms, 0.0, 2).length == 2)
  }

  test("detectar: el ejemplo 2 reporta (0, 1) con 0.75 y (2, 3) con 0.5") {
    val r = detectarDirecto(c2, 3, 0.3, 5)
    assert(r.map(m => (m._1, m._2)) == Vector((0, 1), (2, 3)))
    assert(math.abs(r(0)._3 - 0.75) < eps && math.abs(r(1)._3 - 0.5) < eps)
  }

  test("detectar: las dos soluciones coinciden en los ejemplos") {
    assert(detectarDirecto(c1, 3, 0.2, 5) == detectarConIndice(c1, 3, 0.2, 5))
    assert(detectarDirecto(c2, 3, 0.3, 5) == detectarConIndice(c2, 3, 0.3, 5))
  }

  //Test parejasCandidatas
  test("parejasCandidatas: un índice vacío no produce parejas") {
    val idx: Indice = Map.empty

    assert(parejasCandidatas(idx).isEmpty)
  }

  test("parejasCandidatas: un documento asociado a un fragmento no forma pareja") {
    val idx: Indice = Map(
      Vector("gato", "duerme") -> Set(4)
    )

    assert(parejasCandidatas(idx).isEmpty)
  }

  test("parejasCandidatas: tres documentos que comparten un fragmento generan tres parejas") {
    val idx: Indice = Map(
      Vector("sol", "brilla") -> Set(1, 3, 5)
    )

    assert(parejasCandidatas(idx) ==
      Vector((1, 3), (1, 5), (3, 5)))
  }

  test("parejasCandidatas: compartir varios fragmentos no duplica parejas") {
    val idx: Indice = Map(
      Vector("la", "luna") -> Set(0, 2),
      Vector("luna", "brilla") -> Set(0, 2)
    )

    assert(parejasCandidatas(idx) == Vector((0, 2)))
  }

  test("parejasCandidatas: ordena las parejas y evita invertir sus índices") {
    val idx: Indice = Map(
      Vector("rio", "fluye") -> Set(8, 3),
      Vector("mar", "calma") -> Set(6, 1)
    )

    assert(parejasCandidatas(idx) ==
      Vector((1, 6), (3, 8)))
  }

  // Test ranking
  test("ranking: descarta mediciones inferiores al umbral") {
    val ms: Vector[Medicion] = Vector(
      (0, 1, 0.2),
      (0, 2, 0.8),
      (1, 2, 0.5),
      (2, 3, 0.1)
    )

    assert(ranking(ms, 0.5, 10) ==
      Vector((0, 2, 0.8), (1, 2, 0.5)))
  }

  test("ranking: incluye mediciones exactamente iguales al umbral") {
    val ms: Vector[Medicion] = Vector(
      (0, 1, 0.4),
      (0, 2, 0.6),
      (1, 2, 0.7)
    )

    assert(ranking(ms, 0.4, 10) ==
      Vector((1, 2, 0.7), (0, 2, 0.6), (0, 1, 0.4)))
  }

  test("ranking: desempata por los índices de los documentos") {
    val ms: Vector[Medicion] = Vector(
      (3, 5, 0.8),
      (1, 4, 0.8),
      (1, 2, 0.8),
      (2, 3, 0.8)
    )

    assert(ranking(ms, 0.5, 10) ==
      Vector(
        (1, 2, 0.8),
        (1, 4, 0.8),
        (2, 3, 0.8),
        (3, 5, 0.8)
      ))
  }

  test("ranking: devuelve solamente los primeros p resultados") {
    val ms: Vector[Medicion] = Vector(
      (0, 1, 0.3),
      (0, 2, 0.9),
      (1, 2, 0.6),
      (2, 3, 0.7)
    )

    assert(ranking(ms, 0.0, 2) ==
      Vector((0, 2, 0.9), (2, 3, 0.7)))
  }

  test("ranking: devuelve un vector vacío cuando p es cero o negativo") {
    val ms: Vector[Medicion] = Vector(
      (0, 1, 0.9),
      (1, 2, 0.7)
    )

    assert(ranking(ms, 0.0, 0).isEmpty)
    assert(ranking(ms, 0.0, -2).isEmpty)
  }

  // Test detectarDirecto
  test("detectarDirecto: un corpus vacío no produce resultados") {
    val c: Corpus = Vector.empty

    assert(detectarDirecto(c, 2, 0.1, 5).isEmpty)
  }

  test("detectarDirecto: un único documento no genera comparaciones") {
    val c: Corpus = Vector(
      ("d0", Vector("sol", "brilla", "hoy"))
    )

    assert(detectarDirecto(c, 2, 0.0, 5).isEmpty)
  }

  test("detectarDirecto: dos documentos idénticos tienen similitud uno") {
    val c: Corpus = Vector(
      ("d0", Vector("sol", "brilla", "hoy")),
      ("d1", Vector("sol", "brilla", "hoy"))
    )

    assert(detectarDirecto(c, 2, 1.0, 5) ==
      Vector((0, 1, 1.0)))
  }

  test("detectarDirecto: dos documentos sin fragmentos compartidos tienen similitud cero") {
    val c: Corpus = Vector(
      ("d0", Vector("sol", "brilla", "hoy")),
      ("d1", Vector("luna", "duerme", "noche"))
    )

    assert(detectarDirecto(c, 2, 0.0, 5) ==
      Vector((0, 1, 0.0)))

    assert(detectarDirecto(c, 2, 0.1, 5).isEmpty)
  }

  test("detectarDirecto: aplica el umbral y limita la cantidad de resultados") {
    val c: Corpus = Vector(
      ("d0", Vector("a", "b", "c")),
      ("d1", Vector("a", "b", "c")),
      ("d2", Vector("a", "b", "d")),
      ("d3", Vector("x", "y", "z"))
    )

    assert(detectarDirecto(c, 2, 0.5, 1) ==
      Vector((0, 1, 1.0)))
  }

  // Test detectarConIndice
  test("detectarConIndice: un corpus vacío no produce resultados") {
    val c: Corpus = Vector.empty

    assert(detectarConIndice(c, 2, 0.1, 5).isEmpty)
  }

  test("detectarConIndice: documentos sin suficientes palabras no generan candidatas") {
    val c: Corpus = Vector(
      ("d0", Vector("sol")),
      ("d1", Vector("sol")),
      ("d2", Vector("luna"))
    )

    assert(detectarConIndice(c, 2, 0.1, 5).isEmpty)
  }

  test("detectarConIndice: dos documentos con fragmentos compartidos se comparan correctamente") {
    val c: Corpus = Vector(
      ("d0", Vector("a", "b", "c", "d")),
      ("d1", Vector("a", "b", "c", "x"))
    )

    assert(detectarConIndice(c, 2, 0.5, 5) ==
      Vector((0, 1, 0.5)))
  }

  test("detectarConIndice: excluye documentos sin fragmentos compartidos") {
    val c: Corpus = Vector(
      ("d0", Vector("sol", "brilla", "hoy")),
      ("d1", Vector("luna", "duerme", "noche")),
      ("d2", Vector("mar", "mueve", "olas"))
    )

    assert(detectarConIndice(c, 2, 0.1, 5).isEmpty)
  }

  test("detectarConIndice: coincide con el detector directo en un corpus con varias candidatas") {
    val c: Corpus = Vector(
      ("d0", Vector("a", "b", "c", "d")),
      ("d1", Vector("a", "b", "c", "x")),
      ("d2", Vector("a", "b", "y", "z")),
      ("d3", Vector("m", "n", "o", "p"))
    )

    val esperado: Vector[Medicion] = Vector(
      (0, 1, 0.5),
      (0, 2, 0.2),
      (1, 2, 0.2)
    )

    assert(detectarConIndice(c, 2, 0.15, 10) == esperado)

    assert(
      detectarConIndice(c, 2, 0.15, 10) ==
      detectarDirecto(c, 2, 0.15, 10)
    )
  }

}
