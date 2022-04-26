import std/os/file
import std/os/path

type cellValue
  Float(i: float64)
  String(s: string)
  Task(t: () -> cellValue)

effect state
  fun fetch(k: string) : cellValue // maybe<cellValue>
  fun put(k: string, v: cellValue) : ()
  fun print-spreadsheet() : console 


fun load-spreadsheet()
  put("A1", Float(1.0))
  put("A4", Float(1.0))

fun calculate-index(key : string) : int
  list(key).map(int).sum - 65 - 49

fun init-spreadsheet(size: int, spreadsheet: list<maybe<cellValue>>) : _e list<maybe<cellValue>>
  if(size <= 0)
  then spreadsheet
  else init-spreadsheet(size - 1, [Nothing] ++ spreadsheet)


fun handle-app(action : () -> state a): <console|_e> a
  var spreadsheet := init-spreadsheet(10, [])
  with handler
    fun put(k, v)
      val idx = calculate-index(k)
      val split_ss = spreadsheet.split(idx)
      val tail = split_ss.snd.drop(0)
      spreadsheet := split_ss.fst ++ tail
    fun fetch(k)
      // TODO: Implement fetch
      Float(1.0)
    fun print-spreadsheet()
      println("Yet to be implemented")
  action()

fun main()
  handle-app(load-spreadsheet)