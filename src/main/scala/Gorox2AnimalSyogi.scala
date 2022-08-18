package gorox2animalsyogi

import scala.collection.mutable
import Console.{GREEN, BLUE, RESET}

sealed trait Gorox2Error

case object FallOutFromField extends Gorox2Error // コマを移動させる際に、盤外に落ちてしまうようなコマの移動先を指定した
case object ImpposibleMotion extends Gorox2Error // コマを移動させる際に、選択したコマでは不可能な移動先を指定した
case object NonExistingField extends Gorox2Error // 移動したいコマを選択する際に、盤外が指定された
case object NonExistingKoma extends Gorox2Error // 移動したいコマを選択する際に、指定された座標にコマが存在しなかった
case object SelectEnemyKoma extends Gorox2Error // 移動したいコマを選択する際に、敵チームのコマを指定した
case object SelectOwnKoma extends Gorox2Error // コマを移動させる際に、既に味方チームのコマがいる移動先を指定した

class Gorox2AnimalSyogi(pfName: String, dfName: String) {
  private class Player(val name: String, val color: String) {
    val ownKoma: mutable.Map[Char, Int] = mutable.Map('H' -> 0, 'N' -> 0, 'I' -> 0)
    
    def getEnemyKoma(icon: Char): Unit = ownKoma += (icon -> (ownKoma(icon) + 1))
  }

  sealed abstract class Koma {
    val icon: Char
    val color: String
    val movable: List[(Int, Int)]
  }

  case class Raion(val color: String) extends Koma {
    val icon = 'R'
    val movable = List((1, 0), (1, 1), (0, 1), (-1, 1), (-1, 0), (-1, -1), (0, -1), (1, -1))
  }

  case class Inu(val color: String) extends Koma {
    val icon = 'I'
    val movable = List((1, 0), (1, 1), (0, 1), (-1, 1), (-1, 0), (0, -1))
  }

  case class Neko(val color: String) extends Koma {
    val icon = 'N'
    val movable = List((1, 1), (0, 1), (-1, 1), (-1, -1), (1, -1))
  }

  case class Hiyoko(val color: String) extends Koma {
    val icon = 'H'
    val movable = List((0, 1))
  }

  case class Tori(val color: String) extends Koma {
    val icon = 'T'
    val movable = List((1, 0), (1, 1), (0, 1), (-1, 1), (-1, 0), (0, -1))
  }

  private final val FIELD_SIZE = 35

  private val playFirst: Player = new Player(pfName, GREEN)

  private val drawFirst: Player = new Player(dfName, BLUE)

  private var reverse = false

  private val mainField: Array[Array[Koma]] = Array(
    Array(Neko(BLUE), Inu(BLUE), Raion(BLUE), Inu(BLUE), Neko(BLUE)),
    Array(null, null, null, null, null),
    Array(null, Hiyoko(BLUE), Hiyoko(BLUE), Hiyoko(BLUE), null),
    Array(null, Hiyoko(GREEN), Hiyoko(GREEN), Hiyoko(GREEN), null),
    Array(null, null, null, null, null),
    Array(Neko(GREEN), Inu(GREEN), Raion(GREEN), Inu(GREEN), Neko(GREEN))
  )

  def moveKoma(nowPos: (Int, Int), nextPos: (Int, Int)): Either[Gorox2Error, Unit] = {
    val nowPlayerColor = if (reverse) drawFirst.color else playFirst.color
    val (nowPosX, nowPosY) = nowPos
    val (nextPosX, nextPosY) = nextPos
    val diffPosX = if (reverse) nextPosX - nowPosX else (nextPosX - nowPosX) * (-1)
    val diffPosY = if (reverse) nextPosY - nowPosY else (nextPosY - nowPosY) * (-1) 

    if (nowPosX < 0 || nowPosX > 4 || nowPosY < 0 || nowPosY > 5) return Left(NonExistingField)
    val nowKoma = mainField(nowPosY)(nowPosX) match {
      case koma: Koma => if (nowPlayerColor == koma.color) koma else return Left(SelectEnemyKoma)
      case _ => return Left(NonExistingKoma)
    }
    if (!(nowKoma.movable.contains((diffPosX, diffPosY)))) return Left(ImpposibleMotion)
    if (nextPosX < 0 || nextPosX > 4 || nextPosY < 0 || nextPosY > 5) return Left(FallOutFromField)
    mainField(nextPosY)(nextPosX) match {
      case koma: Koma => {
        if (nowPlayerColor != koma.color) koma else return Left(SelectOwnKoma)
        if (reverse) drawFirst.getEnemyKoma(koma.icon) else playFirst.getEnemyKoma(koma.icon)
      }
      case _ => null
    }
    mainField(nowPosY)(nowPosX) = null
    mainField(nextPosY)(nextPosX) = nowKoma
    Right(())
  }

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
      s"${" " * ((FIELD_SIZE - 21) / 2 - 2)}${i + 1}${mainFieldLine.map[String] {
        koma => koma match {
          case _: Koma => s"${koma.color}${koma.icon}${RESET}"
          case _ => s"${" "}"
        }
      }.mkString(" | ", " | ", " |")}\n"
    }.mkString(s"${" " * ((FIELD_SIZE - 21) / 2)}${"-" * (FIELD_SIZE - 14)}\n")
  }

  private def makeSubField(player: Player): String = {
    s"${player.color}${player.name}" + 
    s"${" " * (FIELD_SIZE - player.name.length - 11)}" + 
    s"${(player.ownKoma.map[String] { case (icon, count) => s"${icon}:${count}" }).mkString(" ")}" + 
    s"${RESET}\n"
  }
}