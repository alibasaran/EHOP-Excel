import std/os/file
import std/os/path
import helpers

// Idea: for calculating dependencies run tasks under different handler

type cellValue
  Float(i: float64)
  // String(s: string)
  Task(t: () -> cellValue)

effect state
  fun fetch(k: string) : float64 // maybe<cellValue>
  fun put(k: string, v: cellValue) : ()
  fun print-spreadsheet() : ()

effect commands
  fun sum(a: string, b: string) : float64
  
effect tasks
  fun eval(v: maybe<cellValue>) : float64

val task-handler = handler
  fun eval(v)
    match v
      Nothing -> 0.0
      Just(cell-val: cellValue) -> match cell-val
        Float(i: float64) -> i
        Task(_) -> 0.0

val command-handler = handler
  fun sum(a, b)
    fetch(a) + fetch(b)

pub fun handle-app(n-rows : int, n-columns : int, action : () -> <console, exn, state, commands> a): <console, exn |_e> a
  var spreadsheet := replicate(replicate(Nothing, n-columns), n-rows)
  with task-handler
  with handler
    fun put(k, v)
      val idx = calculate-index(k)
      match spreadsheet[idx.fst]
        Nothing -> () // Expand spreadsheet
        Just(l : list<maybe<cellValue>>) -> {
          val row-to-append = replace-at-index(idx.snd, Just(v), l)
          spreadsheet := replace-at-index(idx.fst, row-to-append, spreadsheet)
        }
    fun fetch(k)
      val idx = calculate-index(k)
      match spreadsheet[idx.fst]
        Nothing -> throw("Requested cell is empty")
        Just(row : list<maybe<cellValue>>) -> match row[idx.snd]
          Nothing -> throw("Requested cell is empty")
          Just(v : maybe<cellValue>) -> eval(v)
    fun print-spreadsheet()
      print("\t")
      list('A', char(int('A') + n-columns - 1)).foreach(fn (col) print(string(col) ++ "\t"))
      println("")
      with x <- list(1, n-rows).foreach
      print(x); print("\t")
      match spreadsheet[x - 1]
        Just(l : list<maybe<cellValue>>) -> l.foreach(fn (elem) {print(eval(elem)); print("\t")})
        Nothing -> ()
      println("")
  with command-handler
  action()

fun load-spreadsheet()
  put("A1", Float(1.0))
  put("A4", Float(1.0))
  print-spreadsheet()
  println("Results:")
  println(sum("A1", "A4"))
  ()

fun main()
  handle-app(4, 4, load-spreadsheet)