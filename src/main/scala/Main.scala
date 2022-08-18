import gorox2animalsyogi.{Gorox2AnimalSyogi => G2as}

object Main extends App {
  val g2as = new G2as("Penguin", "Tokage")
  g2as.changeTurn()
  g2as.moveKoma((2, 2), (2, 3)) match {
    case Right(_) => println("Success: MoveKoma Succeed!!")
    case Left(e) => println(s"Error: ${e}!!")
  }
  println(g2as.makeField(false))
}