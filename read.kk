import std/os/file
import std/os/path

fun main()
  val fp = path("./values.txt")
  println(fp.show())
  val file-txt = read-text-file(fp)
  print-list(main)