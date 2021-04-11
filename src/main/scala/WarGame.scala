import enumeratum._
import enumeratum.values.{IntEnum, IntEnumEntry}

import scala.util.Random

object WarGame extends App {

  sealed abstract class Rank(val value: Int, val name: String) extends IntEnumEntry

  object Rank extends IntEnum[Rank] {

    case object Two   extends Rank(1, "Two")
    case object Three extends Rank(2, "Three")
    case object Four  extends Rank(3, "Four")
    case object Five  extends Rank(4, "Five")
    case object Six   extends Rank(5, "Six")
    case object Seven extends Rank(6, "Seven")
    case object Eight extends Rank(7, "Eight")
    case object Nine  extends Rank(8, "Nine")
    case object Ten   extends Rank(9, "Ten")
    case object Jack  extends Rank(10, "Jack")
    case object Queen extends Rank(11, "Queen")
    case object King  extends Rank(12, "King")
    case object Ace   extends Rank(13, "Ace")

    val values = findValues
  }

  sealed trait Suite extends EnumEntry

  object Suite extends Enum[Suite] {
    val values = findValues

    case object Hearts   extends Suite
    case object Spades   extends Suite
    case object Clubs    extends Suite
    case object Diamonds extends Suite

  }

  case class Card(rank: Rank, suite: Suite)
  case class Player(hand: Set[Card], scorePile: Set[Card])
  case class State(player1: Player, player2: Player)

  sealed trait GameResult
  case object Player1Wins extends GameResult
  case object Player2Wins extends GameResult
  case object Draw        extends GameResult

  def showResult(result: GameResult) =
    println(result match {
      case Player1Wins => "Player 1 Wins!"
      case Player2Wins => "Player 2 Wins!"
      case Draw        => "It's a draw!"
    })

  def makeDeck(): Set[Card] =
    (for {
      s <- Suite.values
      r <- Rank.values
    } yield Card(r, s)).toSet

  println(makeDeck())
  println(makeDeck().size)

  def makeHands(): (Set[Card], Set[Card]) = {
    val shuffled = Random.shuffle(makeDeck())

    val half = shuffled.size / 2

    val player1Hand = shuffled.take(half)
    val player2Hand = shuffled.drop(half)

//    println(player1Hand)
    println(player1Hand.size)

//    println(player2Hand)
    println(player2Hand.size)

    (player1Hand, player2Hand)
  }

  val hands = makeHands()

  val (hand1, hand2) = hands

  def initialState(player1Hand: Set[Card], player2Hand: Set[Card]): State =
    State(Player(player1Hand, Set()), Player(player2Hand, Set()))

  val initState = initialState(hand1, hand2)

  val rand = scala.util.Random
  val trumpSuite = Suite.values(rand.nextInt(4))

  def next(state: State): State = {
    val player1Hand = state.player1.hand.toList
    val player2Hand = state.player2.hand.toList
    var player1ScorePile = state.player1.scorePile
    val player2ScorePile = state.player2.scorePile

    for (x <- 0 to player1Hand.size - 1) {
      val player1Card = player1Hand(x)
      val player2Card = player2Hand(x)

      if (player1Card.suite == trumpSuite && player2Card.suite == trumpSuite) {
        if (player1Card.rank.value > player2Card.rank.value) {
          player1ScorePile += player1Card
          player1ScorePile += player2Card
        }
      }
    }
  }

  def play(initialState: State): GameResult = ???
}

// 1. set up a Scala "hello world" project
// 2. implement the enum types
// 3. implement makeHands method
// 4. implement initialState creation method
// then we'll talk about how to do a recursive `next`

// enums are in enumeratum library https://github.com/lloydmeta/enumeratum
// libraryDependencies ++= Seq(
//    "com.beachape" %% "enumeratum" % "1.6.1"
// )
