import gorox2animalsyogi.{Gorox2AnimalSyogi => G2as}

object Main extends App {
  val g2as = new G2as("Penguin", "Tokage")
  g2as.moveKoma((2, 3), (2, 2)) match {
    case Right(o) => o match {
      case Some(e) => println(s"Success: ${e}!!")
      case None => println("Success: MoveKoma Succeed!!")
    }
    case Left(e) => println(s"Error: ${e}!!")
  }
  g2as.moveKoma((2, 2), (2, 1)) match {
    case Right(o) => o match {
      case Some(e) => println(s"Success: ${e}!!")
      case None => println("Success: MoveKoma Succeed!!")
    }
    case Left(e) => println(s"Error: ${e}!!")
  }
  g2as.upgradeKoma((2, 1)) match {
    case Right(_) => println("Success: UpgradeKoma Succeed!!")
    case Left(e) => println(s"Error: ${e}!!")
  }
  println(g2as.makeField(false))
}