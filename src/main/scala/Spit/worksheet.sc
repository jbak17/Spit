import Spit.spit

var outstring: String = " "
println(outstring)
outstring += "foo "
outstring += "bar"
println(outstring)

val strList: List[String] = List("foo", "bar", "what")

val newstring: String = strList.foldLeft("")(_ + " " + _)

val str: String = "."*2
var card = spit.cardToString(12, "D")


