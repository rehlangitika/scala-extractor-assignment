# scala-extractor-assignment

1. Complete Extractor definition for URL parsing.
object URL{
def
apply(protocal:String,domain:String,path:String,
params:Map[String,String]):String = {
//definition
}
def unapply(url:String):Option[(String , String,
String,Map[String,String])] = {
//definition
}
}
Description :
val url = “https://aws.amazon.com/console/home?
state=hash&isauthcode=true&code=112”
Extract following fields:
protocal =” https”
domain = “aws.amazon.com”
path =”/console/home”
params = Map(“state” -> “hash”, “isauthcode” -> “true”, “code” -> 112)

2. Write regular expression for email parsing.
Description :
val EMAIL = “ regular expression definition”.r
val EMAIL(user, domain) = “knol@knoldus.com”
user = knol
domain = knoldus.com
