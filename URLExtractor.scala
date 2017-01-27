//URL Parsing

object URL {

  def apply(protocol: String, domain: String, path: String, params: Map[String, String]): String = {
    val urlString = s"$protocol://$domain/$path?state=${params.get("state")}&isauthcode=${params.get("isauthcode")}&code=${params.get("code")}"
    urlString
  }

  def unapply(url: String): Option[(String, String, String, Map[String, String])] = {
    if (url.contains("://")) {
      val protocolAndRest = url.split("://")
      val protocol = protocolAndRest(0)
      val domainAndRest = protocolAndRest(1).split("/", 2)
      val domain = domainAndRest(0)

      //when path and params both are present
      if (domainAndRest.length == 2 && domainAndRest(1).contains('/') && domainAndRest(1).contains('?')) {
        val pathAndRest = domainAndRest(1).split("\\?")
        val path = s"/${pathAndRest(0)}"
        val params = for (p <- pathAndRest(1).split("&")) yield {
          (p.split("=")(0), p.split("=")(1))
        }
        Some(protocol, domain, path, params.toMap)
      }

      //when URL contains path but no parameters
      else if (domainAndRest.length == 2 && (domainAndRest(1).contains('/') && !domainAndRest(1).contains('?'))) {
        val pathAndRest = domainAndRest(1).split("/")
        val path = s"/${pathAndRest(0)}/${pathAndRest(1)}"
        val params = Map("" -> "")
        Some(protocol, domain, path, params.toMap)
      }

      //when both path and params are not present
      else {
        val path = ""
        val params = Map("" -> "")
        Some(protocol, domain, path, params.toMap)
      }
    }

    //if it is not a valid URL
    else {
      None
    }
  }
}

object URLExtractor {
  def main(args: Array[String]): Unit = {
    val p = Map("state" -> "hash", "isauthcode" -> "true", "code" -> "112")
    val url1 = "https://aws.amazon.com/console/home?state=hash&isauthcode=true&code=112"
    val url2 = "https://aws.amazon.com"
    val url3 = "https://aws.amazon.com/console/home"

    def parseURL(url: String): String = {
      url match {
        case URL(protocol, domain, path, params) => "Match: Protocol: " + protocol + " Domain: " + domain + " Path: " + path + " Params: " + params
        case _ => "No Match Found"
      }
    }
    println(parseURL(url1))
    println(parseURL(url2))
    println(parseURL(url3))
  }
}
