package Spit

import Spit.spit._
import akka.actor.ActorSystem
import akka.dispatch.PriorityGenerator
import com.typesafe.config.Config
import akka.dispatch.UnboundedStablePriorityMailbox


/**
  * Created by jeva on 17/05/17.
  */
class SpitMailbox (settings: ActorSystem.Settings, config: Config) extends UnboundedStablePriorityMailbox(

    //lower priority means treated first
    PriorityGenerator {
      case Handover => 0
      case DeclaresVictory => 2
      case Endgame => 4
      case SendCard =>6
      case AcceptCard => 8
      case RejectCard => 8
      case _ => 20
    }

  )

