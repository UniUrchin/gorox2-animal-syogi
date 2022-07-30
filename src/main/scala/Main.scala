import scala.collection.mutable
import Console.{GREEN, BLUE, RESET}

class Gorox2AnimalSyogi(pfColor: String, dfColor: String) {
  import Koma._

  private abstract class Koma {
    val icon: Char
    val color: String
  }

  private object Koma {
    def raion(color: String): Koma = 
      new RaionKoma(color)

    def inu(color: String): Koma = 
      new InuKoma(color)

    def neko(color: String): Koma = 
      new NekoKoma(color)

    def hiyoko(color: String): Koma = 
      new HiyokoKoma(color)

    def tori(color: String): Koma = 
      new ToriKoma(color)

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

  private val mainField: Array[Array[Koma]] = Array(
    Array(neko(dfColor), inu(dfColor), raion(dfColor), inu(dfColor), neko(dfColor)),
    Array(null, null, null, null, null),
    Array(null, hiyoko(dfColor), hiyoko(dfColor), hiyoko(dfColor), null),
    Array(null, hiyoko(pfColor), hiyoko(pfColor), hiyoko(pfColor), null),
    Array(null, null, null, null, null),
    Array(neko(pfColor), inu(pfColor), raion(pfColor), inu(pfColor), neko(pfColor))
  )

  def makeField(nowPlayer: Player, otherPlayer: Player): String = {        
    s"${"=" * 35}\n" + 
    s"${otherPlayer.color}${otherPlayer.name}${" " * (24 - otherPlayer.name.length)}" + 
    s"${(otherPlayer.own_koma.map[String] {
      case (icon, count) => s"${icon}:${count}"
    }).mkString(" ")}${RESET}\n" + 
    s"${"=" * 35}\n\n" + 
    s"${" " * 9}${"ABCDE".mkString("   ")}\n" + 
    s"${" " * 7}${"-" * 21}\n" + 
    mainField.zipWithIndex.map[String] {
      case (mainFieldLine, i) => s"${" " * 5}${i + 1} |${mainFieldLine.map[String] {
        koma => koma match {
          case _: Koma => s" ${koma.color}${koma.icon}${RESET} "
          case _ => s"${" " * 3}"
        }
      }.mkString("|")}|\n"
    }.mkString(s"${" " * 7}${"-" * 21}\n") + 
    s"${" " * 7}${"-" * 21}\n" + 
    s"\n${"=" * 35}\n" + 
    s"${nowPlayer.color}${(nowPlayer.own_koma.map[String] {
      case (icon, count) => s"${icon}:${count}"
    }).mkString(" ")}" + 
    s"${" " * (24 - nowPlayer.name.length)}${nowPlayer.name}${RESET}\n" + 
    s"${"=" * 35}\n"
  }
}

class Player(val name: String, val color: String) {
  val own_koma: mutable.Map[Char, Int] = mutable.Map('H' -> 0, 'N' -> 0, 'I' -> 0)
}

object Main extends App {
  val playFirst = new Player("Penguin", GREEN)
  val drawFirst = new Player("Tokage", BLUE)

  val g2as = new Gorox2AnimalSyogi(playFirst.color, drawFirst.color)
  println(g2as.makeField(playFirst, drawFirst))
}