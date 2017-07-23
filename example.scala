import scades._

/** Simulation of an MM1 queuing system. */
class MM1(val lambda: Double, val mu: Double) {
  def nextArrivalTime(from: Time) = from + Time(RNG.negExp(lambda))
  def nextServiceTime(from: Time) = from + Time(RNG.negExp(mu))

  val sim = new Simulation

  object generator extends Process {
    override def handle = {
      case Event(Generate, now, me, _) =>
        println(s"Generator activated at $now.")
        sim add Event(Job, now, handler = server, issuer = me)
        sim add Event(Generate, nextArrivalTime(from = now), me, me)
      }
  }

  object server extends Process {
    private var nbrOfJobsInQ = 0
    def n = nbrOfJobsInQ

    override def handle = {
      case Event(Job, now, me, _) =>
        println(s"Server starts processing Job at $now. In queue: $nbrOfJobsInQ")
        if (nbrOfJobsInQ == 0)
          sim add Event(JobDone, nextServiceTime(from = now), me, me)
        nbrOfJobsInQ += 1

      case Event(JobDone, now, me, _) =>
        nbrOfJobsInQ -= 1
        println(s"Server JobDone at $now. In queue: $nbrOfJobsInQ")
        if (nbrOfJobsInQ > 0)
          sim add Event(JobDone, nextServiceTime(from = now), me, me)
    }
  }

  def simulate(duration: Time) = {
    sim init Event(Generate, handler = generator)
    sim until duration
  }
}

object MainMM1 {
  def main(args: Array[String]): Unit =
    new MM1(
      lambda = args.lift(0).getOrElse("10.0").toDouble,
      mu     = args.lift(1).getOrElse("8.0").toDouble
    ).simulate(duration=Time(args.lift(2).getOrElse("100.0").toDouble))
}
