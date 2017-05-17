package Spit

import Spit.spit._
import akka.actor.ActorSystem
import akka.dispatch.PriorityGenerator
import com.typesafe.config.Config
import akka.dispatch.UnboundedStablePriorityMailbox


/**
  * Created by jeva on 17/05/17.
  */
class DealerMailbox(settings: ActorSystem.Settings, config: Config) extends UnboundedStablePriorityMailbox(

    //lower priority means treated first
    PriorityGenerator {
      case DeclaresVictory => 0

      case SendCard =>5
      case Handover => 1
      case Endgame => 4
      case _ => 20
    }

  )



