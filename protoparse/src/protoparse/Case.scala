package protoparse

object Case {
  private val Snake = "_([a-z])".r
  def snakeToCamel(snake: String): String = {
    val m = Snake.pattern.matcher(snake)
    val sb = new StringBuffer
    while (m.find()) {
      m.appendReplacement(
        sb,
        m.group().charAt(1).toUpper + m.group().substring(2)
      )
    }
    m.appendTail(sb)
    val result = sb.toString
    pprint.log(snake)
    pprint.log(result)
    result
  }

}
