package week6


case class Pouring(val capacities : Vector[Int], val goalVolume : Int) {
  type State = Vector[Int];
  val initialState = capacities map (_ => 0)
  val glasses = 0 until initialState.length
  
  trait Action {
  	def change(state: State) : State
  	def isApplicable(state : State) : Boolean
  }
  
  case class Fill(glass : Int) extends Action {
	def change(state: State) = {
	  state updated(glass, capacities(glass))
	}
	
	def isApplicable(state : State) = {
	  state(glass) < capacities(glass)
	}
  }
  
  case class Empty(glass : Int) extends Action {
  	def change(state: State) = {
	  state updated(glass, 0)
	}
	
	def isApplicable(state : State) = {
	  state(glass) > 0
	}
  }
  
  case class Pour(from : Int, to : Int) extends Action {
  	def change(state: State) = {
  	  val fromInitVolume = state(from)
  	  val toInitVolumne = state(to)
	  val pouredVolume = (capacities(to) - toInitVolumne) min fromInitVolume
	  state updated(from, fromInitVolume - pouredVolume) updated(to, toInitVolumne + pouredVolume) 
	}
	
	def isApplicable(state : State) = {
	  from != to && state(from) > 0
	}
  }
  
  case class Path(history: List[Action], endState : State) {
  	def applyAction(action : Action) : Path = {
  	  Path(action::history, action change endState) 	
  	}
  	override def toString = ((history reverse)  mkString " ") + "-->" + endState + ", size = " + history.size + "\n" 
  }
  
  val initialPath = Path(Nil, initialState)
  
  val actions = 
  	(for (glass <- glasses) yield Fill(glass)) ++
  	(for (glass <- glasses) yield Empty(glass)) ++
  	(for (from <- glasses; to <- glasses if from != to) yield Pour(from, to))
  	
  
  def applicableActions(state : State) = actions filter (_.isApplicable(state))
  
  def from(paths : Set[Path], explored: Set[State]) : Stream[Set[Path]] = {
  	if(paths.isEmpty) Stream.Empty
  	else {
  	  val more  = 
  	  	for {
  	      path <- paths
  	      next <- applicableActions(path.endState) map path.applyAction
  	      if !(explored contains next.endState)
  	    } yield next
  	  paths#::from(more, explored ++ more.map(_.endState))
  	}	
  }
  
  def getPathStream(paths : Set[Path], explored: Set[State]) : Stream[Path] = {
  	if(paths.isEmpty) Stream.Empty
  	else {
  	  val more  = 
  	  	for {
  	      path <- paths
  	      next <- applicableActions(path.endState) map path.applyAction
  	      if !(explored contains next.endState)
  	    } yield next
  	  paths.toStream #::: getPathStream(more, explored ++ more.map(_.endState))
  	}
  }
  
  //val pathsStream = from(Set(initialPath), Set(initialState))
  val pathsStream = getPathStream(Set(initialPath), Set(initialState))
  
  def solutions() : Stream[Path] = {
  	for {
  	  path <- pathsStream
  	  if path.endState contains goalVolume
  	} yield path
  }
  	
   
}