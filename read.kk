import std/os/file
import std/os/path

fun main()
  val fp = path("./values.txt")
  println(fp.show())
  val main: string = read-text-file(fp)
  println(main)