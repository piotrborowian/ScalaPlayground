package alternative
import scala.collection.mutable

trait Parameters {
  def andDelay : Int
  def orDelay : Int
  def invertDelay : Int
}

trait Action extends Parameters {	
  def run() : Unit
}

case class Event(time : Int, action : Action)

trait Simulation {  
  private var agenda = List.empty[Event]
  private var curtime = 0
  
  def currentTime: Int = curtime
  
  def schedule(event : Event) {
  	def priorityInsert(event : Event, agenda : List[Event]) : List[Event] = agenda match {
      case first :: rest if first.time <= event.time => first :: priorityInsert(event, rest)
      case _ => event :: agenda
    }
  	agenda = priorityInsert(event, agenda);
  }
  
  def afterDelay(delay: Int)(action : Action): Unit = {
    val event = Event(currentTime + delay, action)
    schedule(event)
  }
  
}

class WireBuilder(name : String) {
  val actions = mutable.ListBuffer.empty[Action]
  def add(action : Action) {
  	actions += action
  }
  def build = Wire(name, actions.toList)
}

case class Wire(name : String, actions : List[Action]) {
  private var _state = false
  def state_= (newState : Boolean) {
  	if( _state != newState) {
  	  actions foreach (_.run)  
  	  _state = newState	
  	}
  }
  def state = _state
}

case class And(in1: Wire, in2: Wire, out: Wire) {
  def run() {
  	out.state = in1.state && in2.state
  }
}

case class Or(in1: Wire, in2: Wire, out: Wire) {
  def run() {
  	out.state = in1.state || in2.state
  }	
}

case class Invert(in : Wire, out: Wire) {
  def run() {
  	out.state = !in.state
  }
}

