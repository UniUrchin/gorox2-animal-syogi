class Gorox2AnimalSyogi(playFirst: Player, drawFirst: Player) {
    import Koma._

    private abstract class Koma {
        val icon: Char
        val side: Player
    }

    private object Koma {
        def lion(side: Player): Koma = 
            new LionKoma(side)

        def dog(side: Player): Koma = 
            new DogKoma(side)

        def cat(side: Player): Koma = 
            new CatKoma(side)

        def chick(side: Player): Koma = 
            new ChickKoma(side)

        def chicken(side: Player): Koma = 
            new ChickenKoma(side)

        private class LionKoma(val side: Player) extends Koma {
            val icon = 'ら'
        }

        private class DogKoma(val side: Player) extends Koma {
            val icon = 'い'
        }

        private class CatKoma(val side: Player) extends Koma {
            val icon = 'ね'
        }

        private class ChickKoma(val side: Player) extends Koma {
            val icon = 'ひ'
        }

        private class ChickenKoma(val side: Player) extends Koma {
            val icon = 'に'
        }
    }

    private var nowPlayer = playFirst
    private var mainField: Array[Array[Koma]] = Array(
        Array(cat(drawFirst), dog(drawFirst), lion(drawFirst), dog(drawFirst), cat(drawFirst)),
        Array(null, null, null, null, null),
        Array(null, chick(drawFirst), chick(drawFirst), chick(drawFirst), null),
        Array(null, chick(playFirst), chick(playFirst), chick(playFirst), null),
        Array(null, null, null, null, null),
        Array(cat(playFirst), dog(playFirst), lion(playFirst), dog(playFirst), cat(playFirst))
    )
    private var subField: Array[Array[Koma]] = Array(
        Array(null, null, null, null),
        Array(null, null, null, null),
        Array(null, null, null, null),
        Array(null, null, null, null)
    )
}

class Player(val name: String, val color: String)

object Main extends App {
    val playFirst = new Player("Penguin", "Green")
    val drawFirst = new Player("Tokage", "Blue")

    val g2as = new Gorox2AnimalSyogi(playFirst, drawFirst)
}