class Gorox2AnimalSyogi(playFirst: Player, drawFirst: Player) {
    private abstract class Koma {

    }

    private class LionKoma extends Koma {
        
    }

    private var nowPlayer = playFirst
    private var mainField = Array.ofDim[Koma](5, 6)
    private var subField = Array.ofDim[Koma](4, 4)
}

class Player(val name: String, val color: String)

object Main extends App {
    val playFirst = new Player("Penguin", "Green")
    val drawFirst = new Player("Tokage", "Blue")
    val g2as = new Gorox2AnimalSyogi(playFirst, drawFirst)
}