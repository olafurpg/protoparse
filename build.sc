import mill._
import mill.scalalib._

object protoparse extends ScalaModule {
  def scalaVersion = "2.12.4"

  override def mainClass = Some("protoparse.Protoparse")

  def ivyDeps = Agg(
    ivy"org.scala-lang:scala-reflect:${scalaVersion()}",
    ivy"com.lihaoyi::pprint:0.5.3",
    ivy"com.lihaoyi::fastparse:1.0.0"
  )
}

