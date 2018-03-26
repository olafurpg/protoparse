package protoparse

import java.nio.file.Files
import java.nio.file.Paths

object Main {
  def main(args: Array[String]): Unit = {
    val Array(in, out) = args
    val ast = Protoparse.proto
      .parse(new String(Files.readAllBytes(Paths.get(in))))
      .get
      .value
    pprint.log(ast)
  }
}
