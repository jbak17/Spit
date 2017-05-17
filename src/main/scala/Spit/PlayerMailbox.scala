package Spit

import Spit.spit._
import akka.actor.ActorSystem
import akka.dispatch.{PriorityGenerator, UnboundedStablePriorityMailbox}
import com.typesafe.config.Config

/**
  * Created by jeva on 17/05/17.
  */
class PlayerMailbox (settings: ActorSystem.Settings, config: Config) extends UnboundedStablePriorityMailbox(

  //lower priority means treated first
  PriorityGenerator {

    case RejectCard => 0 //we want this added back to the stack if dealer was considering when end was called
    case Handover => 5 //dealer calling cease of actions
    case BuildLayout => 6
    case AcceptCard => 10
    case SendCard =>15
    case Table => 16
    case EndOfStream => 16
    case _ => 20
  }

)
