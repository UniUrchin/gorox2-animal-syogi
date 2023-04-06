package gorox2animalsyogi

import scala.collection.mutable
import Console.{GREEN, BLUE, RESET}

sealed trait Gorox2Event // なんかある時の通知に使いたい

case object UpgradePossible extends Gorox2Event // コマが成れる条件を満たす移動を行った
case object CatchRion extends Gorox2Event // コマが成れる条件を満たす移動を行った

sealed trait Gorox2Error

case object AlreadyExistingKoma extends Gorox2Error // コマを配置する際に、既にコマが存在する配置先を指定した
case object FallOutFromField extends Gorox2Error // コマを移動させる際に、盤外に落ちてしまうようなコマの移動先を指定した
case object ImpposibleMotion extends Gorox2Error // コマを移動させる際に、選択したコマでは不可能な移動先を指定した
case object NonExistingIcon extends Gorox2Error // コマを配置する際に、存在しないアイコンを指定した
case object NonExistingKoma extends Gorox2Error // 移動したいコマを選択する際に、指定された座標にコマが存在しなかった
case object NonExistingOwnKoma extends Gorox2Error // コマを配置する際に、1つも持っていないコマを指定した
case object SelectEnemyKoma extends Gorox2Error // 移動したいコマを選択する際に、敵チームのコマを指定した
case object SelectOwnKoma extends Gorox2Error // コマを移動させる際に、既に味方チームのコマがいる移動先を指定した
case object SpecifiedCannotUpgradeKoma extends Gorox2Error // コマを成らせる際に、成ることができないコマが指定された
case object SpecifiedOutSideBoard extends Gorox2Error // 移動したいコマ、もしくは配置先を選択する際に、盤外が指定された

class Gorox2AnimalSyogi(pfName: String, dfName: String) {
  private class Player(val name: String, val color: String) {
    val ownKoma: mutable.Map[Char, Int] = mutable.Map('H' -> 0, 'N' -> 0, 'I' -> 0)
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

  case class Sakana(val color: String) extends Koma {
    val icon = 'S'
    val movable = List((1, 0), (1, 1), (0, 1), (-1, 1), (-1, 0), (-1, -1), (0, -1), (1, -1))
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

  private val playFirst: Player = new Player(pfName.take(FIELD_SIZE - 12), GREEN)
  private val drawFirst: Player = new Player(dfName.take(FIELD_SIZE - 12), BLUE)
  private var nowPlayer: Player = playFirst
  private var upgradeKomaPos: Option((Int, Int)) = None
  private val mainField: Array[Array[Koma]] = Array(
    Array(Neko(drawFirst.color), Inu(drawFirst.color), Raion(drawFirst.color), Inu(drawFirst.color), Neko(drawFirst.color)),
    Array(null, null, null, null, null),
    Array(null, Hiyoko(drawFirst.color), Hiyoko(drawFirst.color), Hiyoko(drawFirst.color), null),
    Array(null, Hiyoko(playFirst.color), Hiyoko(playFirst.color), Hiyoko(playFirst.color), null),
    Array(null, null, null, null, null),
    Array(Neko(playFirst.color), Inu(playFirst.color), Raion(playFirst.color), Inu(playFirst.color), Neko(playFirst.color))
  )

  def moveKoma(nowPos: (Int, Int), nextPos: (Int, Int)): Either[Gorox2Error, Option[Gorox2Event]] = {
    val (nowPosX, nowPosY) = nowPos
    val (nextPosX, nextPosY) = nextPos
    val diffPosX = if (nowPlayer == playFirst) (nextPosX - nowPosX) * (-1) else nextPosX - nowPosX
    val diffPosY = if (nowPlayer == playFirst) (nextPosY - nowPosY) * (-1) else nextPosY - nowPosY 

    if (nowPosX < 0 || nowPosX >= 5 || nowPosY < 0 || nowPosY >= 6) return Left(SpecifiedOutSideBoard)
    val nowKoma = mainField(nowPosY)(nowPosX) match {
      case koma: Koma => if (nowPlayer.color == koma.color) koma else return Left(SelectEnemyKoma)
      case _ => return Left(NonExistingKoma)
    }
    if (!(nowKoma.movable.contains((diffPosX, diffPosY)))) return Left(ImpposibleMotion)
    if (nextPosX < 0 || nextPosX >= 5 || nextPosY < 0 || nextPosY >= 6) return Left(FallOutFromField)
    mainField(nextPosY)(nextPosX) match {
      case koma: Koma => {
        if (nowPlayer.color == koma.color) return Left(SelectOwnKoma)
        if (nowPlayer == playFirst) {
          koma.icon match {
            case 'R' => return Right(Some(CatchRion))
            case 'T' => playFirst.ownKoma += ('H' -> (playFirst.ownKoma('H') + 1))
            case 'S' => playFirst.ownKoma += ('N' -> (playFirst.ownKoma('N') + 1))
            case _ => playFirst.ownKoma += (koma.icon -> (playFirst.ownKoma(koma.icon) + 1))
          }
        } else {
          koma.icon match {
            case 'R' => return Right(Some(CatchRion))
            case 'T' => drawFirst.ownKoma += ('H' -> (drawFirst.ownKoma('H') + 1))
            case 'S' => drawFirst.ownKoma += ('N' -> (drawFirst.ownKoma('N') + 1))
            case _ => drawFirst.ownKoma += (koma.icon -> (drawFirst.ownKoma(koma.icon) + 1))
          }
        }
      }
      case _ => ()
    }
    mainField(nowPosY)(nowPosX) = null
    mainField(nextPosY)(nextPosX) = nowKoma
    if upgradeCheck(nowKoma.icon, nowPosY, nextPosY) {
      upgradeKomaPos = ((nextPosY, nextPosX))
      return Right(Some(UpgradePossible))
    }
    Right(None)
  }

  def placeKoma(icon: Char, nextPos: (Int, Int)): Either[Gorox2Error, Unit] = {
    val (nextPosX, nextPosY) = nextPos
    val nextKoma = icon match {
      case 'H' => Hiyoko(nowPlayer.color)
      case 'I' => Inu(nowPlayer.color)
      case 'N' => Neko(nowPlayer.color)
      case _ => return Left(NonExistingIcon)
    }

    if (nextPosX < 0 || nextPosX >= 5 || nextPosY < 0 || nextPosY >= 6) return Left(SpecifiedOutSideBoard)
    mainField(nextPosY)(nextPosX) match {
      case koma: Koma => return Left(AlreadyExistingKoma)
      case _ => {
        if (nowPlayer == playFirst) {
          if (playFirst.ownKoma(icon) == 0) return Left(NonExistingOwnKoma)
          playFirst.ownKoma += (icon -> (playFirst.ownKoma(icon) - 1))
        } else {
          if (drawFirst.ownKoma(icon) == 0) return Left(NonExistingOwnKoma)
          drawFirst.ownKoma += (icon -> (drawFirst.ownKoma(icon) - 1))
        }
        mainField(nextPosY)(nextPosX) = nextKoma
      }
    }
    Right(())
  }
  
  // 成れるコマのリストをメンバに持たせて消す方が早そう
  def upgradeKoma(nowPos: (Int, Int)): Either[Gorox2Error, Unit] = {
    val (nowPosX, nowPosY) = nowPos

    if (nowPosX < 0 || nowPosX >= 5 || nowPosY < 0 || nowPosY >= 6) return Left(SpecifiedOutSideBoard)
    mainField(nowPosY)(nowPosX) match {
      case _: Hiyoko => mainField(nowPosY)(nowPosX) = Tori(nowPlayer.color)
      case _: Neko => mainField(nowPosY)(nowPosX) = Sakana(nowPlayer.color)
      case _ => return Left(SpecifiedCannotUpgradeKoma)
    }
    Right(())
  }

  def changeTurn(): Unit = {
    nowPlayer = if (nowPlayer == playFirst) drawFirst else playFirst
    mainField.foreach
  }

  private def upgradeCheck(icon: Char, nowPosY: Int, nextPosY: Int): Boolean = {
    icon match {
      case 'H' | 'N' => {
        if (nowPlayer == playFirst) {
          if ((nowPosY == 2 && nextPosY == 1) || (nowPosY == 1 && nextPosY == 2) || (nowPosY <= 1 && nextPosY <= 1)) true else false
        } else {
          if ((nowPosY == 3 && nextPosY == 4) || (nowPosY == 4 && nextPosY == 3) || (nowPosY >= 4 && nextPosY >= 4)) true else false
        }
      }
      case _ => false
    }
  }

  def makeField(reverse: Boolean): String = {
    var strField = new String

    strField += s"${"=" * FIELD_SIZE}\n"
    strField += s"${if (reverse) makeSubField(playFirst) else makeSubField(drawFirst)}"
    strField += s"${"=" * FIELD_SIZE}\n"
    strField += s"\n"
    strField += s"${" " * ((FIELD_SIZE - 17) / 2)}${(if (reverse) "EDCBA" else "ABCDE").mkString("   ")}\n"
    strField += s"${" " * ((FIELD_SIZE - 21) / 2)}${"-" * (FIELD_SIZE - 14)}\n"
    strField += makeMainField(reverse)
    strField += s"${" " * ((FIELD_SIZE - 21) / 2)}${"-" * (FIELD_SIZE - 14)}\n"
    strField += s"\n"
    strField += s"${"=" * FIELD_SIZE}\n"
    strField += s"${if (reverse) makeSubField(drawFirst) else makeSubField(playFirst)}"
    strField += s"${"=" * FIELD_SIZE}\n"
    strField
  }

  private def makeMainField(reverse: Boolean): String = {
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