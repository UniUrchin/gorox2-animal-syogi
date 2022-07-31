import scala.collection.mutable
import Console.{GREEN, BLUE, RESET}

class Gorox2AnimalSyogi(
    private val playFirst: Player, 
    private val drawFirst: Player) {
  import Koma._

  private abstract class Koma {
    val icon: Char
    val color: String
  }

  private object Koma {
    def raion(color: String): Koma = new RaionKoma(color)

    def inu(color: String): Koma = new InuKoma(color)

    def neko(color: String): Koma = new NekoKoma(color)

    def hiyoko(color: String): Koma = new HiyokoKoma(color)

    def tori(color: String): Koma = new ToriKoma(color)

    private class RaionKoma(val color: String) extends Koma {
      val icon = 'R'
    }

    private class InuKoma(val color: String) extends Koma {
      val icon = 'I'
    }

    private class NekoKoma(val color: String) extends Koma {
      val icon = 'N'
    }

    private class HiyokoKoma(val color: String) extends Koma {
      val icon = 'H'
    }

    private class ToriKoma(val color: String) extends Koma {
      val icon = 'T'
    }
  }

  private final val FIELD_SIZE = 35

  private var reverse = false

  private val mainField: Array[Array[Koma]] = Array(
    Array(neko(BLUE), inu(BLUE), raion(BLUE), inu(BLUE), neko(BLUE)),
    Array(null, null, null, null, null),
    Array(null, hiyoko(BLUE), hiyoko(BLUE), hiyoko(BLUE), null),
    Array(null, hiyoko(GREEN), hiyoko(GREEN), hiyoko(GREEN), null),
    Array(null, null, null, null, null),
    Array(neko(GREEN), inu(GREEN), raion(GREEN), inu(GREEN), neko(GREEN))
  )

  // def moveKoma(bx: Int, by: Int, ax: Int, ay: Int): Bool {
  //   if mainField(bx)(by).isMovable(bx, by, ax, ay) {
  //     match 
  //     mainField(ax)(ay) = mainField(bx)(by)
  //     mainField(bx)(by) = null
  //   } else {
  //     false
  //   }
  // }

  def makeField: String = {
    var strField = new String
    strField += s"${"=" * FIELD_SIZE}\n"
    strField += s"${if (reverse) makeSubField(playFirst) else makeSubField(drawFirst)}"
    strField += s"${"=" * FIELD_SIZE}\n"
    strField += s"\n"
    strField += s"${" " * ((FIELD_SIZE - 17) / 2)}${(if (reverse) "EDCBA" else "ABCDE").mkString("   ")}\n"
    strField += s"${" " * ((FIELD_SIZE - 21) / 2)}${"-" * (FIELD_SIZE - 14)}\n"
    strField += makeMainField
    strField += s"${" " * ((FIELD_SIZE - 21) / 2)}${"-" * (FIELD_SIZE - 14)}\n"
    strField += s"\n"
    strField += s"${"=" * FIELD_SIZE}\n"
    strField += s"${if (reverse) makeSubField(drawFirst) else makeSubField(playFirst)}"
    strField += s"${"=" * FIELD_SIZE}\n"
    strField
  }

  private def makeMainField: String = {
    val mainField = if (reverse) this.mainField.zipWithIndex.reverse else this.mainField.zipWithIndex
    mainField.map[String] { case (line, i) => 
      val mainFieldLine = if (reverse) line.reverse else line
      s"${" " * ((FIELD_SIZE - 21) / 2 - 2)}${i + 1} |${mainFieldLine.map[String] {
        koma => koma match {
          case _: Koma => s" ${koma.color}${koma.icon}${RESET} "
          case _ => s"${"   "}"
        }
      }.mkString("|")}|\n"
    }.mkString(s"${" " * ((FIELD_SIZE - 21) / 2)}${"-" * (FIELD_SIZE - 14)}\n")
  }

  private def makeSubField(player: Player): String = {
    s"${player.color}${player.name}" + 
    s"${" " * (FIELD_SIZE - player.name.length - 11)}" + 
    s"${(player.own_koma.map[String] { case (icon, count) => s"${icon}:${count}" }).mkString(" ")}" + 
    s"${RESET}\n"
  }
}

class Player(val name: String, val color: String) {
  val own_koma: mutable.Map[Char, Int] = mutable.Map('H' -> 0, 'N' -> 0, 'I' -> 0)
}

object Main extends App {
  val playFirst = new Player("Penguin", GREEN)
  val drawFirst = new Player("Tokage", BLUE)
  val g2as = new Gorox2AnimalSyogi(playFirst, drawFirst)
  println(g2as.makeField)
}