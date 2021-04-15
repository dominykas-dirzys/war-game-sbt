import enumeratum._
import enumeratum.values.{IntEnum, IntEnumEntry}

import scala.annotation.tailrec
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

  case class Card(rank: Rank, suite: Suite) {
    def value(trumpSuite: Suite): Int =
      if (suite == trumpSuite) rank.value + 13
      else rank.value
  }

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

  def makeHands(): (Set[Card], Set[Card]) = {
    val shuffled = Random.shuffle(makeDeck())

    val half = shuffled.size / 2

    val player1Hand = shuffled.take(half)
    val player2Hand = shuffled.drop(half)

    (player1Hand, player2Hand)
  }

  val hands = makeHands()

  val (hand1, hand2) = hands

  def initialState(player1Hand: Set[Card], player2Hand: Set[Card]): State =
    State(Player(player1Hand, Set()), Player(player2Hand, Set()))

  val initState = initialState(hand1, hand2)

  val rand       = scala.util.Random
  val trumpSuite = Suite.values(rand.nextInt(4))
  println("Trump suite is - " + trumpSuite)

  def next(state: State): State = {
    var player1Hand      = state.player1.hand
    var player2Hand      = state.player2.hand
    var player1ScorePile = state.player1.scorePile
    var player2ScorePile = state.player2.scorePile

    val player1Card = player1Hand.head
    val player2Card = player2Hand.head

    if (player1Card.value(trumpSuite) > player2Card.value(trumpSuite)) {
      println("P1 " + player1Card + " beats P2 " + player2Card)
      player1ScorePile += player1Card
      player1ScorePile += player2Card
    } else if (player1Card.value(trumpSuite) == player2Card.value(trumpSuite)) {
      println("P1 " + player1Card + " ties with P2 " + player2Card)
      player1ScorePile += player1Card
      player2ScorePile += player2Card
    } else {
      println("P2 " + player2Card + " beats P1 " + player1Card)
      player2ScorePile += player1Card
      player2ScorePile += player2Card
    }

    player1Hand -= player1Card
    player2Hand -= player2Card

    State(Player(player1Hand, player1ScorePile), Player(player2Hand, player2ScorePile))
  }

  def play(initialState: State): GameResult = {
    val rounds: Int = initialState.player1.hand.size
    @tailrec def tailRecPlay(state: State, n: Int): State = {
      if (n < 1)
        state
      else
        tailRecPlay(next(state), n - 1)
    }
    val endState = tailRecPlay(initialState, rounds)

    val p1Score = endState.player1.scorePile.size
    val p2Score = endState.player2.scorePile.size

    println("Player 1 scored: " + p1Score)
    println("Player 2 scored: " + p2Score)

    if (p1Score > p2Score)
      Player1Wins
    else if (p1Score < p2Score)
      Player2Wins
    else
      Draw
  }

  showResult(play(initState))
}