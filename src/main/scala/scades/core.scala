package scades

case class Event(
  msg: Message     = Empty,
  at: Time         = Time(0.0),
  handler: Process = EmptyHandler,
  issuer:  Process = NoProcess)
object EmptyEvent extends Event()

case class Time(time: Double) extends AnyVal with Ordered[Time] {
  def +(delta: Time): Time = Time(time + delta.time)
  override def compare(that: Time): Int = (this.time - that.time).toInt
}

trait Message  // not sealed: use as base type for your own messages
case class  Text(msg: String)     extends Message
case class  Job[T](data: T)       extends Message
object      Job                   extends Job(())
case class  JobDone[T](report: T) extends Message
object      JobDone               extends JobDone(())
case object Generate              extends Message
case object Empty                 extends Message

class NoHandlerException(msg: String = "")  extends Exception(msg)
class BeforeTimeException(msg: String = "") extends Exception(msg)

trait Process { def handle: PartialFunction[Event, Unit] }
case object EmptyHandler extends Process { def handle = { case _ => }}
case object NoProcess    extends Process {
  def handle = throw new NoHandlerException("illegal to call NoProcess.handle")
}

class Simulation {
  private val eventQueue = collection.mutable.PriorityQueue.empty[Event](
    Ordering.fromLessThan[Event]((e1, e2) => e1.at.time > e2.at.time)
  )

  def queueLength = eventQueue.length
  def isMoreEvents: Boolean = queueLength > 0
  def events: Stream[Event] = eventQueue.toStream

  private var currentTime: Double = 0.0
  def now: Time = Time(currentTime)
  def nextTimestamp = eventQueue.head.at.time

  def add(event: Event): Unit =
    if (event.at.time >= currentTime) eventQueue.enqueue(event)
    else throw new BeforeTimeException(s"$event is before $now")

  var onHandleEvent: Event => Unit = e => ()   // callback plugin

  def handleNextEvent(): Unit = {
    val event = eventQueue.dequeue
    currentTime = event.at.time
    onHandleEvent(event)
    event.handler.handle(event)
  }

  def until(endOf: Time): Unit =
    while (isMoreEvents && nextTimestamp <= endOf.time) handleNextEvent()

  def init(initEvent: Event = Event()): Unit = {
    eventQueue.clear()
    currentTime = 0.0
    add(initEvent)
  }
}
