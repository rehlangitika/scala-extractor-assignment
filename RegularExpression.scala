import java.util.Scanner

class RegularExpression {

  /*
  cases taken -
  1. only !_- are allowed as special symbols in user name
  2. user name cannot start with digit or any special character
  3. domain name can be of length between 2 and 63
  4. consecutive periods(.) should not be there in domain name
  */

  val EMAIL =
  """^([a-zA-Z][a-zA-Z0-9\.]*[!_-]*)@((?:[a-z0-9]+[\.])+[a-z0-9]{2,63})""".r

  def findAll(array: Array[String], list: List[String]) = {
    for (arr <- array; s <- EMAIL findAllIn (arr)) yield {
      s :: list
    }
  }

}

object RegularExpression {

  def main(args: Array[String]) = {

    val regExp = new RegularExpression
    val scanner = new Scanner(System.in)
    println("Enter email \n")
    val email = scanner.next()

    email match {
      case regExp.EMAIL(user, domain) => println("user = " + user + "\ndomain = " + domain)
      case _ => println("not a valid email")
    }
  }

}
