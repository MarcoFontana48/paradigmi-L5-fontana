package polyglot.a01b

import polyglot.{OptionToOptional, Pair}
import util.Optionals.Optional as ScalaOptional
import util.Sequences.Sequence

import scala.util.Random

/** solution and descriptions at https://bitbucket.org/mviroli/oop2019-esami/src/master/a01b/sol2/ */
class LogicsImpl(private val size: Int, private val mines: Int) extends Logics:
  private var minesSequence: Sequence[Pair[Int, Int]] = Sequence.empty
  private var selected: Sequence[Pair[Int, Int]] = Sequence.empty
  private val random = Random()

  while (minesSequence.size() < mines) //added size method for ex03
    val minePositionX = random.nextInt(size)
    val minePositionY = random.nextInt(size)
    if !minesSequence.contains(Pair(minePositionX, minePositionY)) then minesSequence = minesSequence.concat(Sequence(Pair(minePositionX, minePositionY)))

  def hit(x: Int, y: Int): java.util.Optional[Integer] =
    if minesSequence.contains(Pair(x, y)) then OptionToOptional(ScalaOptional.Empty())
    else
      selected = selected.concat(Sequence(Pair(x, y)))
      val minesAround = minesSequence.filter(p => Math.abs(p.getX - x) <= 1 && Math.abs(p.getY - y) <= 1).size()
      OptionToOptional(ScalaOptional.Just(minesAround))

  def won: Boolean = selected.size() + minesSequence.size() == size * size