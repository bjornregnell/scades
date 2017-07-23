package scades

/** Random Number Generator (RNG) */
object RNG {
  /** Underlying JDK thread-safe random number generator. */
  val jrand = java.util.concurrent.ThreadLocalRandom.current

  /** RNG with exponetial distribution. */
  def negExp(mean: Double)        = math.log(1 - jrand.nextDouble)/(-1 / mean)

  /** RNG with rectangular distribution. */
  def rect(from: Int, until: Int) = jrand.nextInt(from, until)

  /** Set the seed for repeatable pseudo-random sequence. */
  def setSeed(seed: Long)         = jrand.setSeed(seed)
}
