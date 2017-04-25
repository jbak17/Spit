import Spit.spit.Dealer
import akka.actor.{ActorSystem, Props}

val system = ActorSystem("Spit_System")
//create dealer
val dealer = system.actorOf(Props[Dealer],"Dealer")

println(dealer.path)

