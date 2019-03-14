package sudoku

/**
  * Created by alvaro on 4/03/17.
  */
class SudokuPorDeduccion {

  // http://www.conceptispuzzles.com/index.aspx?uri=puzzle/sudoku/techniques
  // https://www.sudokuoftheday.com/dailypuzzles/

  def log(s: String) = {
    //System.err.println(s)
  }

  type Numero = Int

  trait Dominio {
    var posibles = scala.collection.mutable.Set[Numero]()

    def posible(n: Numero) = posibles.contains(n)

    def definido = cardinalidad == 1

    def vacio = cardinalidad == 0

    def cardinalidad = posibles.size

    def toSet : Set[Int] = Set[Int](posibles.toSeq: _*)

    def valor = definido match {
      case true => Some(posibles.head)
      case false => None
    }

    override def toString() = {
      (1 to 9).map(i => if( !posible(i) ) "_" else i.toString ).mkString(",")
    }
  }

  class DominioDecreciente extends Dominio {


    (1 to 9).foreach( posibles += _ )

    def marcaComoImposibles(s: Iterable[Numero]) = posibles --= s

    def marcaComoImposible(n: Numero*) = marcaComoImposibles(n)

    def marcaComoValor(n: Numero) = marcaComoImposible((1 to 9).filter(_ != n): _*)
  }

  class DominioCreciente extends Dominio {

    def marcaComoPosible(n: Numero*) = posibles ++= n

    def ++=(d: Dominio) = posibles ++= d.toSet
  }

  class Grupo(val nombre: String, val celdas: Seq[Celda]){
    for(c<-celdas) c.grupos.add(this)
  }

  class Celda(val fila:Int, val columna:Int) {
    import scala.collection.mutable.{Set=>MSet}
    val grupos = MSet[Grupo]()
    val dominio = new DominioDecreciente()
    override def toString() = dominio.toString
  }

  val celdas = Array.tabulate(9, 9)(new Celda(_,_))

    // FILAS
    val filas = (0 until 9).map { fila =>
      new Grupo(s"Fila ${fila+1}", Array.tabulate(9)(columna => celdas(fila)(columna)))
    }

    // COLUMNAS
    val columnas = (0 until 9).map { columna =>
      new Grupo(s"Columna ${columna+1}", Array.tabulate(9)(fila => celdas(fila)(columna)))
    }

    // CUADRADOS
    val cuadrados = {
      for (fila <- 0 until 3; columna <- 0 until 3) yield {
        new Grupo(s"Cuadrado ${fila+1},${columna+1}", for (c <- 0 until 3; f <- 0 until 3) yield {
          celdas(fila * 3 + f)(columna * 3 + c)
        })

      }
    }

  val grupos: Seq[Grupo] = {

    filas ++ columnas ++ cuadrados
  }

  def valoresIniciales(valores: Array[Array[Int]]) = {
    assert(valores.length == 9)
    for (f <- valores.indices; c <- valores(f).indices) {
      assert(valores(f).length == 9)
      val v = valores(f)(c)
      assert(v >= 0 && v <= 9)
      //log(s"Leido $v como valor de $f,$c")
      if (v != 0) {
        celdas(f)(c).dominio.marcaComoValor(v)
      }
    }
  }

  def imprimeSolucion() = {
    for (f <- 0 until 9) {
      if( f%3 == 0 ) println()
      for (c <- 0 until 9) {
        val s = celdas(f)(c).dominio.valor match{
          case Some(n) => n.toString
          case None => "_"
        }
        val sep = if( c%3 == 0 ) " " else ""
        print(  sep + s + " ")
      }
      println()
    }
  }

  def imprimeDominios() = {
    for (f <- 0 until 9) {
      for (c <- 0 until 9) {
        print(celdas(f)(c).dominio + "\t")
      }
      println()
    }
  }

  def solucion() : Array[Array[Numero]]= {
    celdas.map( _.map(_.dominio.valor.getOrElse(0)))
  }

  def definido = celdas.flatten.forall( _.dominio.definido )


  def reduceDominioPorCeldasDefinidas() = reduceDominioPorCircunscripcion(1 to 1)

  def reduceDominioPorCircunscripcion(g: Grupo, size: Numero) = {
    var ret = false
    for (s <- g.celdas.toSet.subsets(size)) {
      val d = new DominioCreciente
      s.foreach(c => d ++= c.dominio)
      if (d.cardinalidad == size) {
        val imposibles = d.toSet
        //log( s"size:$size subset:$s")
        //log( s"  posibilidades:$d")
        //log( s"    marcando como imposibles en el resto del grupo")
        for (c <- (g.celdas.toSet -- s); n <- imposibles intersect c.dominio.toSet) {
          c.dominio.marcaComoImposible(n)
          println(s"La celda:${c.fila + 1},${c.columna + 1} no puede tener el valor $n por el subconjunto $d en el grupo ${g.nombre}")
          ret = true
        }
      }
    }
    ret
  }

  def reduceDominioPorCircunscripcion(sizes:Seq[Int] = (1 until 8)) : Boolean = {
    val ret = for(g <- grupos; size <- sizes ) yield {
      reduceDominioPorCircunscripcion(g, size)
    }
    ret.exists(_==true)
  }

  def reduceDominioPorCircunscripcionOculta( size: Int, g: Grupo ) : Boolean = {
    var ret = false
    val subconjuntos = g.celdas.foldLeft(Set[Set[Int]]()){ (s:Set[Set[Int]],c) =>
      s ++ c.dominio.toSet.subsets(size)
    }
    log( s"size:$size grupo:${g.nombre} subconjuntos:$subconjuntos")
    for( s <- subconjuntos ){
      log( s"  subjconjunto:$s")
      val celdas = g.celdas.filter( c => s.subsetOf( c.dominio.toSet) )
      log( s"  celdas con $s: $celdas")
      lazy val otrasCeldas = g.celdas.toSet -- celdas
      log( s"  celdas sin $s: $otrasCeldas")
      lazy val algunValorEnOtrasCeldas = otrasCeldas.exists( c => c.dominio.toSet.intersect(s).size > 0 )
      if( celdas.size == s.size && !algunValorEnOtrasCeldas ){
        for( c <- celdas ; v <- c.dominio.toSet -- s ) {
          println( s"la celda ${c.fila+1},${c.columna+1} no puede tener el valor $v por el subconjunto $s en el grupo ${g.nombre}")
          c.dominio.marcaComoImposible(v)
          ret = true
        }
        for( c <- otrasCeldas ; v <- s if( c.dominio.posible(v) )){
          println( s"la celda ${c.fila+1},${c.columna+1} no puede tener el valor $v por el subconjunto $s en el grupo ${g.nombre}")
          c.dominio.marcaComoImposible(v)
          ret = true
        }
      }
    }
    ret

  }


  def reduceDominioPorCircunscripcionOculta(sizes:Seq[Int] = (1 until 9) ) : Boolean = {
    val ret = for( size <- sizes ; g <- grupos ) yield {
      reduceDominioPorCircunscripcionOculta(size, g)
    }
    ret.exists(_==true)
  }
}

class SudokuPorBacktracking(base: Array[Array[Int]]){

  private val valores = Array.fill(9,9)(0)

  type Grupo = Array[Celda]

  class Celda(f:Int,c:Int){

    def valor = {
      base(f)(c) match{
        case 0 => valores(f)(c)
        case n => n
      }
    }

    def apply() = valor
    def update(n:Int) = valores(f)(c) = n

    var grupos:Seq[Grupo] = Seq()

    def valida = grupos.forall(grupoValido)
  }

  private val celdas = Array.tabulate(9,9)( (f,c) => new Celda(f,c) )
  private val grupos: Array[Grupo] = {
    val filas = Array.tabulate(9)(f => Array.tabulate(9)(c => celdas(f)(c)))
    val columnas = Array.tabulate(9)(c => Array.tabulate(9)(f => celdas(f)(c)))
    val cuadros = Array.tabulate(9) { i =>
        val fila = i / 3
        val columna = i % 3
        Array.tabulate(9)(j => celdas(fila * 3 + j/3)(columna * 3 + j%3))
      }
    filas ++ columnas ++ cuadros
  }

  for( f <- 0 until 9 ; c <- 0 until 9 ){
    val celda =celdas(f)(c)
    celda.grupos = grupos.filter( _.contains(celda) )
  }

  private def grupoValido(g: Grupo ) : Boolean = {
    val valores = g.map(_.valor)
    (1 to 9).map( v => valores.count(_==v) ).forall(_<=1)
  }


  def valido() = grupos.forall(grupoValido)


  def resuelto = valido && grupos.forall(_.forall(_.valor>0) )

  def solve(i:Int = 0, maximo:Int=0) : (Boolean,Int) = {
    if( i > maximo ) {
      //println(s"\n\nCELDA $i")
      //imprimeSolucion()
    }

    var maximoLocal = maximo max i
    val fila = i / 9
    val columna = i % 9

    if( i >= 9*9 ) {
      (true, maximoLocal)
    }
    else if( celdas(fila)(columna).valor != 0 ){
      solve(i + 1, maximoLocal)
    }
    else {

      for( v <- 9 to 1 by -1 if(!resuelto) ){
        valores(fila)(columna) = v
        if( celdas(fila)(columna).valida ) {
          val r = solve(i + 1, maximoLocal)
          maximoLocal = r._2
        }
      }
      if( !resuelto )
        valores(fila)(columna) = 0
      (false,maximoLocal max i)
    }
  }

  def imprimeSolucion() = {
    for (f <- 0 until 9) {
      if( f%3 == 0 ) println()
      for (c <- 0 until 9) {
        val s = celdas(f)(c).valor
        val sep = if( c%3 == 0 ) " " else ""
        print(  sep + s + " ")
      }
      println()
    }
  }


}


object Main extends App {
  /*
  sudoku(
    Array(
      Array(1, 2, 3, 4, 5, 6, 7, 8, 9),
      Array(0, 0, 0, 0, 0, 0, 0, 0, 0),
      Array(0, 0, 0, 0, 0, 0, 0, 0, 0),

      Array(0, 0, 0, 0, 0, 0, 0, 0, 0),
      Array(0, 0, 0, 0, 0, 0, 0, 0, 0),
      Array(0, 0, 0, 0, 0, 0, 0, 0, 0),

      Array(0, 0, 0, 0, 0, 0, 0, 0, 0),
      Array(0, 0, 0, 0, 0, 0, 0, 0, 0),
      Array(0, 0, 0, 0, 0, 0, 0, 0, 0)
    )
  )
  */
  val facil = Array(
    Array(0,0,8, 0,0,0, 0,6,4),
    Array(0,0,2, 0,5,4, 0,0,0),
    Array(0,4,0, 0,6,0, 0,0,7),

    Array(0,0,6, 0,0,0, 8,0,0),
    Array(0,8,5, 0,0,0, 2,0,0),
    Array(2,3,0, 0,0,5, 4,0,6),

    Array(0,0,4, 2,0,3, 6,0,5),
    Array(0,0,3, 0,0,6, 7,0,8),
    Array(7,6,9, 0,0,0, 0,0,2)
  )

  val muydificil = Array(
    Array(0,0,2, 4,0,8, 3,0,5),
    Array(0,0,3, 7,0,2, 0,0,0),
    Array(4,0,6, 5,0,0, 7,0,8),

    Array(7,0,0, 0,0,0, 0,8,2),
    Array(3,0,5, 0,0,4, 0,7,0),
    Array(0,0,0, 0,0,0, 5,0,6),

    Array(0,0,0, 8,2,0, 9,0,0),
    Array(6,0,0, 0,7,3, 0,5,4),
    Array(0,0,0, 1,0,0, 0,6,7)
  )

  val extremo = Array(
    Array(0,0,0, 0,0,0, 4,1,0),
    Array(7,0,0, 3,0,0, 0,0,0),
    Array(3,0,0, 0,5,0, 0,0,0),

    Array(0,4,9, 0,0,8, 0,0,0),
    Array(0,0,0, 0,0,0, 0,5,2),
    Array(0,1,0, 0,0,0, 0,0,0),

    Array(2,0,0, 6,0,0, 0,0,7),
    Array(0,8,0, 0,0,0, 9,0,0),
    Array(0,0,0, 0,7,0, 0,0,0)
  )

  val hard = Array(
    Array(0,6,9, 0,5,0, 0,1,0),
    Array(0,7,0, 0,0,2, 8,0,9),
    Array(0,0,8, 0,0,6, 0,0,0),

    Array(6,0,0, 0,0,0, 0,4,3),
    Array(0,0,1, 0,0,0, 6,0,0),
    Array(2,8,0, 0,0,0, 0,0,1),

    Array(0,0,0, 5,0,0, 9,0,0),
    Array(9,0,4, 7,0,0, 0,3,0),
    Array(0,1,0, 0,6,0, 5,7,0)
  )

  val ejemploHiddenPairs = Array(
    Array(4,0,0, 3,1,9, 0,0,6),
    Array(0,0,1, 0,0,0, 9,0,0),
    Array(0,6,7, 4,0,0, 0,2,1),

    Array(7,0,0, 0,5,0, 0,0,4),
    Array(0,0,0, 1,4,2, 0,0,0),
    Array(2,0,0, 0,7,0, 0,0,8),

    Array(0,2,0, 0,0,0, 0,6,0),
    Array(0,0,4, 0,0,0, 8,0,0),
    Array(1,0,0, 5,0,8, 0,0,7)
  )

  val diabolical = Array(
    Array(0,0,0, 2,0,1, 0,4,0),
    Array(9,0,6, 0,0,0, 0,0,0),
    Array(0,8,0, 0,0,7, 5,0,0),

    Array(1,0,0, 0,0,0, 7,0,5),
    Array(0,3,0, 4,0,9, 0,1,0),
    Array(8,0,5, 0,0,0, 0,0,3),

    Array(0,0,9, 3,0,0, 0,8,0),
    Array(0,0,0, 0,0,0, 6,0,4),
    Array(0,2,0, 1,0,5, 0,0,0)
  )

  val vacio = Array(
    Array(0,0,0, 0,0,0, 0,0,0),
    Array(0,0,0, 0,0,0, 0,0,0),
    Array(0,0,0, 0,0,0, 0,0,0),

    Array(0,0,0, 0,0,0, 0,0,0),
    Array(0,0,0, 0,0,0, 0,0,0),
    Array(0,0,0, 0,0,0, 0,0,0),

    Array(0,0,0, 0,0,0, 0,0,0),
    Array(0,0,0, 0,0,0, 0,0,0),
    Array(0,0,0, 0,0,0, 0,0,0)
  )


  private def resuelveSudokuPorPasos(datos: Array[Array[Int]]) = {
    val sudoku = new SudokuPorDeduccion()

    sudoku.valoresIniciales(datos)

    sudoku.imprimeDominios()
    println

    while (sudoku.reduceDominioPorCeldasDefinidas) {
      println("CELDAS DEFINIDAS")
      sudoku.imprimeDominios()
      println
    }


    while (sudoku.reduceDominioPorCircunscripcion()) {
      println("CELDAS CIRCUNSCRIPCION")
      sudoku.imprimeDominios()
      println
    }

    while (sudoku.reduceDominioPorCircunscripcionOculta()) {
      println("CELDAS CIRCUNSCRIPCION OCULTA")
      sudoku.imprimeDominios()
      println
    }


    if (sudoku.definido) {
      sudoku.imprimeSolucion()
    }
    else {
      println("PRUEBA Y ERROR")
      val backtrack = new SudokuPorBacktracking(sudoku.solucion)
      backtrack.solve()
      backtrack.imprimeSolucion()
    }
  }

  resuelveSudokuPorPasos(diabolical)
  //pruebaOculta


  private def pruebaOculta = {
    val sudoku = new SudokuPorDeduccion()
    sudoku.valoresIniciales(ejemploHiddenPairs)
    sudoku.reduceDominioPorCeldasDefinidas
    sudoku.imprimeDominios()
    sudoku.reduceDominioPorCircunscripcionOculta(2, sudoku.filas(6))
    println
    sudoku.imprimeDominios()
  }


}
