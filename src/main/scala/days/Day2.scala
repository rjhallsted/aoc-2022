

object Day2 extends Day {
    val day = 2

    trait Shapes {
        def value: Int
    }
    object Shapes {
        case object Rock extends Shapes {
            val value = 1
        }
        case object Paper extends Shapes {
            val value = 2
        }
        case object Scissors extends Shapes {
            val value = 3
        }

        def getShape(s: String): Shapes = s match {
            case "A" => Rock
            case "B" => Paper
            case "C" => Scissors
        }
    }

    trait Outcomes {
        def value: Int
    }
    object Outcomes {
        case object Win extends Outcomes {
            val value = 6
        }
        case object Lose extends Outcomes {
            val value = 0
        }
        case object Draw extends Outcomes {
            val value = 3
        }

        def goal(s: String): Outcomes = s match {
            case "X" => Lose
            case "Y" => Draw
            case "Z" => Win
        }
    }

    def score(opponent: String, goal: String): Int = {
        val opponentShape = Shapes.getShape(opponent)
        val outcome = Outcomes.goal(goal)

        val yourShape = (outcome, opponentShape) match {
            case (Outcomes.Win, Shapes.Rock) => Shapes.Paper
            case (Outcomes.Win, Shapes.Paper) => Shapes.Scissors
            case (Outcomes.Win, Shapes.Scissors) => Shapes.Rock
            case (Outcomes.Lose, Shapes.Rock) => Shapes.Scissors
            case (Outcomes.Lose, Shapes.Paper) => Shapes.Rock
            case (Outcomes.Lose, Shapes.Scissors) => Shapes.Paper
            case (Outcomes.Draw, Shapes.Rock) => Shapes.Rock
            case (Outcomes.Draw, Shapes.Paper) => Shapes.Paper
            case (Outcomes.Draw, Shapes.Scissors) => Shapes.Scissors
        }

        yourShape.value + outcome.value
    }

    def main(input: String): Unit = {
        val pairs = input.split("\n").map(_.split(" "))
        val res = pairs.map(pair => score(pair(0), pair(1))).sum
        println(res)
    }
}