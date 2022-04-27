import std/os/file
import std/os/path

// Idea: for calculating dependencies run tasks under different handler

type cellValue
  Float(i: float64)
  // String(s: string)
  Task(t: () -> cellValue)

effect state
  fun fetch(k: string) : float64 // maybe<cellValue>
  fun put(k: string, v: cellValue) : ()
  fun print-spreadsheet() : ()
  
effect tasks
  fun eval(v: maybe<cellValue>) : float64

/*
Helper functions
*/

fun replace-at-index(idx : int, value : a, l : list<a>) : _e list<a>
  val list-split = l.split(idx)
  val tail = list-split.snd.drop(1)
  list-split.fst ++ [value] ++ tail

fun calculate-index(k: string): _e (int, int)
  val key-encoded = list(k).map(int)
  val row = key-encoded.filter(fn(x) x <= 57 && x > 49).map(fn(x) x - 49).foldl(0, fn(x, y) 10 * x + y)
  val column = key-encoded.filter(fn(x) x >= 65).map(fn(x) x - 65).foldr(0, fn(x, y) 26 * x + y)
  (row, column)

fun init-row(n-columns: int, row: list<maybe<cellValue>>) : _e list<maybe<cellValue>>
  if(n-columns <= 0)
  then row
  else init-row(n-columns - 1, [Nothing] ++ row)

fun init-spreadsheet(n-rows: int, n-columns: int, spreadsheet: list<list<maybe<cellValue>>>) : _e list<list<maybe<cellValue>>>
  if(n-rows <= 0)
  then spreadsheet
  else init-spreadsheet(n-rows - 1, n-columns, [init-row(n-columns, [])] ++ spreadsheet)

// fun init-spreadsheet(size: int, spreadsheet: list<maybe<cellValue>>) : _e list<maybe<cellValue>>
//   if(size <= 0)
//   then spreadsheet
//   else init-spreadsheet(size - 1, [Nothing] ++ spreadsheet)

val task-handler = handler
  fun eval(v)
    match v
      Nothing -> 0.0
      Just(cell-val: cellValue) -> match cell-val
        Float(i: float64) -> i
        Task(_) -> 0.0

fun handle-app(n-rows : int, n-columns : int, action : () -> <state, console> a): <console, exn |_e> a
  var spreadsheet := init-spreadsheet(n-rows, n-columns, [])
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
  action()

fun load-spreadsheet()
  put("A1", Float(1.0))
  put("A4", Float(1.0))
  print-spreadsheet()
  println(fetch("A4"))
  ()

fun main()
  handle-app(4, 4, load-spreadsheet)